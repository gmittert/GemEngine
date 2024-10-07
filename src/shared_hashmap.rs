use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};

#[derive(Debug)]
pub struct SharedHashMapEntry {
    key: AtomicU64,
    val: AtomicU64,
}

#[derive(Debug)]
pub struct SharedHashMap<const N: usize> {
    data: Vec<SharedHashMapEntry>,
    hits: AtomicUsize,
    misses: AtomicUsize,
    conflicts: AtomicUsize,

    accepted: AtomicUsize,
    rejected: AtomicUsize,
    updates: AtomicUsize,
}

/// A very simple lockless hashmap that supports get/set. The map also supports updates, but only
/// updates of values, the key must remain the same. This allows us to update the evaluation of a
/// node if we compute the evaluation to a deeper later on due to reusing the cache between
/// iteration of iterative deepening.
///
/// I can't think of a way to support updates without introducing some sort of read/write in flight
/// bit(s) that readers would have to increment or check for invalidations. Since I'd like to
/// optimize for as cheap reads as possible
///
/// Keys are stored next to the values they are associated with using modulo to find a spot. If the
/// spot is already taken the insert will fail.
unsafe impl<const N: usize> Send for SharedHashMap<N> {}
unsafe impl<const N: usize> Sync for SharedHashMap<N> {}
impl<const N: usize> SharedHashMap<N> {
    pub fn hash_usage(&self) -> usize {
        self.print_stats();
        (1000 * self.accepted.load(Ordering::Relaxed)) / N
    }
    pub fn print_stats(&self) {
        let hits = self.hits.load(Ordering::Relaxed);
        let misses = self.misses.load(Ordering::Relaxed);
        let conflicts = self.conflicts.load(Ordering::Relaxed);
        let accepted = self.accepted.load(Ordering::Relaxed);
        let rejected = self.rejected.load(Ordering::Relaxed);
        let updates = self.updates.load(Ordering::Relaxed);
        println!("Hits:      {}", hits);
        println!("Misses:    {}", misses);
        println!("Conflicts: {}", conflicts);
        println!(
            "Hit Rate:  {:.5}",
            hits as f64 / (hits + misses + conflicts) as f64
        );
        println!("Updates:          {}", updates);
        println!("Accepted:         {}", accepted);
        println!("Rejected:         {}", rejected);
        println!(
            "Acceptance Rate: {:.5}",
            accepted as f64 / (accepted + rejected) as f64
        );
    }
    pub fn new() -> SharedHashMap<N> {
        let mut vec = Vec::with_capacity(N);
        for _ in 0..N {
            vec.push(SharedHashMapEntry {
                key: AtomicU64::new(0),
                val: AtomicU64::new(0),
            });
        }
        SharedHashMap {
            data: vec,
            hits: AtomicUsize::new(0),
            misses: AtomicUsize::new(0),
            conflicts: AtomicUsize::new(0),
            accepted: AtomicUsize::new(0),
            rejected: AtomicUsize::new(0),
            updates: AtomicUsize::new(0),
        }
    }

    pub fn get(&self, k: u64) -> Option<u64> {
        let pos: usize = k as usize % N;
        let SharedHashMapEntry { key, val } = &self.data[pos];
        let header = key.load(Ordering::Acquire);
        if header == 0 {
            self.misses.fetch_add(1, Ordering::Relaxed);
            return None;
        }
        if header != k {
            self.conflicts.fetch_add(1, Ordering::Relaxed);
            return None;
        }
        self.hits.fetch_add(1, Ordering::Relaxed);
        // We have already synchronized on the aquire of the key so this is safe to be a relaxed
        // load on the value. If we observe the desired key, the write of the value in a
        // potentially different thread will be visible to us, and the read of the value here
        // cannot be reordered before the load acquire of the key.
        Some(val.load(Ordering::Relaxed))
    }

    /// Store a new key value pair in the hashmap. `insert` will not overwrite an existing entry,
    /// and will return a failure if something already exists in the desired hashmap position,
    /// regardless of if the key matches.
    ///
    /// returns true if the pair was successfully stored in an empty cell else false.
    pub fn insert(&self, k: u64, v: u64) -> bool {
        // We reserve values of 0 to indicate uninitialized cells. For our use of this hashmap
        // where we store PackedTTEntries which contain a move, these can never be 0. (Consider
        // that a move contains a from an to position, and h1 is the only position that's encoded
        // as 0. Since a move can't be both to and from h1, at least one of "to" or "from" must be
        // non zero).
        assert_ne!(v, 0);

        // Modulo is fine for now, our zorbrist keys are hopefully effectively random. Since we
        // bail immediately if the spot is taken rather than trying to find a new one, we're not
        // worried about bunching or needing a backup method.
        let pos: usize = k as usize % N;
        let SharedHashMapEntry { key, val } = &self.data[pos];

        // Rather than checking the key, when we insert, we immediately check the value. If it's
        // uninitialized, we claim it and overwrite it.
        //
        // The trick we'll rely on is that we're only allowed to replace an unintialized value, and
        // the atomic cmpexchg will ensure that only one writer actually gets to write that value.
        //
        // MEMORY ORDERING:
        // - success: Relaxed is okay here, we only need the write to be atomic. Readers will first
        //            read the key with Acquire, so they will synchronize with our Release when we
        //            write the key. Since they synchronize, if a reader sees our key write, it
        //            will also see our value write, even if the value write is "Relaxed".
        //
        // - failure: On a fail, we don't care about the value we loaded, we know it's not 0, so we
        //            bail no matter what. Admittedly, there might be an improvement opportunity
        //            here where we could check the entry that beat us to it, and if we're deeper
        //            than it for the same position, we automatically replace it, but let's keep
        //            the concerns separate for now and let the caller figure it out.
        if let Err(_) = val.compare_exchange(0, v, Ordering::Relaxed, Ordering::Relaxed) {
            self.rejected.fetch_add(1, Ordering::Relaxed);
            return false;
        }

        // We successfully claimed the value slot. We're the only one allowed to write the key, so
        // we can do so without a cmpxchg. We do need to write with Release semantics so that a
        // getter will synchronize and be sure to see the correct value that we just wrote.
        key.store(k, Ordering::Release);
        self.accepted.fetch_add(1, Ordering::Relaxed);
        true
    }

    /// Update an existing key in the hashmap from a known existing value to a new one.
    ///
    /// Returns the value previously contained in the hashmap. On a success, this will be equal to
    /// `from`.
    pub fn update(&self, k: u64, from: u64, to: u64) -> Result<u64, u64> {
        let pos: usize = k as usize % N;
        let SharedHashMapEntry { key, val } = &self.data[pos];
        let header = key.load(Ordering::Acquire);
        // It's a caller error if the caller tries to update a cell with the wrong key.
        assert_eq!(header, k);

        // We don't really care about the ordering on success or failure here. The data will be
        // valid regardless of whether a reader gets the old or new value.
        match val.compare_exchange(from, to, Ordering::Relaxed, Ordering::Relaxed) {
            Ok(v) => {
                self.updates.fetch_add(1, Ordering::Relaxed);
                Ok(v)
            }
            Err(v) => {
                self.rejected.fetch_add(1, Ordering::Relaxed);
                Err(v)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::SharedHashMap;

    #[test]
    fn insert_get() {
        let map: SharedHashMap<1024> = SharedHashMap::new();
        for i in 0..1024 {
            let val = map.get(i);
            assert_eq!(val, None);
        }
        for i in 2..1026 {
            let val = map.insert(i, i);
            assert_eq!(val, true);
        }
        for i in 2..1026 {
            let val = map.get(i);
            assert_eq!(val, Some(i));
        }
    }

    #[test]
    fn update_different_key() {
        let map: SharedHashMap<1024> = SharedHashMap::new();
        for i in 2..1026 {
            let val = map.insert(i, i);
            assert_eq!(val, true);
        }
        for i in 2..1026 {
            let val = map.insert(i + 1024, i);
            assert_eq!(val, false);
        }
    }

    #[test]
    fn update_same_key() {
        let map: SharedHashMap<1024> = SharedHashMap::new();
        for i in 2..1026 {
            let val = map.insert(i, i);
            assert_eq!(val, true);
        }
        for i in 2..1026 {
            let val = map.update(i, i, i + 1024);
            assert_eq!(val, Ok(i));
        }
        for i in 2..1026 {
            let val = map.get(i);
            assert_eq!(val, Some(i + 1024));
        }
    }

    #[test]
    fn concurrent() {
        let map: Arc<SharedHashMap<10000>> = Arc::new(SharedHashMap::new());
        let r1_map = map.clone();
        let r2_map = map.clone();
        let r3_map = map.clone();
        let r1 = std::thread::spawn(move || {
            for i in 10000..20000 {
                r1_map.insert(i, i);
            }
        });
        let r2 = std::thread::spawn(move || {
            for i in 20000..30000 {
                r2_map.insert(i, i);
            }
        });
        let r3 = std::thread::spawn(move || {
            for i in 30000..40000 {
                r3_map.insert(i, i);
            }
        });
        let _ = r1.join();
        let _ = r2.join();
        let _ = r3.join();
        let mut inserted_vals = 0;
        for i in 3..40000 {
            if let Some(val) = map.get(i) {
                inserted_vals += 1;
                assert_eq!(val, i);
            }
        }
        assert_eq!(inserted_vals, 10000);
    }
}
