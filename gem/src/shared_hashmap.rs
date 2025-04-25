use portable_atomic::AtomicU128;
use std::{
    marker::PhantomData,
    sync::atomic::{AtomicUsize, Ordering},
};

pub trait Encodable {
    fn from_u64(v: u64) -> Self;
    fn to_u64(&self) -> u64;
}

impl Encodable for u64 {
    fn from_u64(v: u64) -> Self {
        v
    }

    fn to_u64(&self) -> u64 {
        *self
    }
}

#[derive(Debug)]
pub struct SharedHashMapEntry<T: Encodable> {
    data: AtomicU128,
    _value: PhantomData<T>,
}

#[derive(Debug)]
pub struct SharedHashMap<T: Encodable, const N: usize> {
    data: Box<[SharedHashMapEntry<T>; N]>,
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
unsafe impl<T: Encodable, const N: usize> Send for SharedHashMap<T, N> {}
unsafe impl<T: Encodable, const N: usize> Sync for SharedHashMap<T, N> {}
impl<T: Encodable, const N: usize> SharedHashMap<T, N> {
    pub fn hash_usage(&self) -> usize {
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
    pub fn new() -> SharedHashMap<T, N> {
        // We use the nightly "new_zeroed" because doing a regular box new causes a stack overflow
        // on non release builds. We also can't just do a `vec![SharedHashMapEntry::new(0,0); N]`
        // because the atomics are not clonable.
        let data = Box::<[SharedHashMapEntry<T>; N]>::new_zeroed();
        let data = unsafe { data.assume_init() };

        SharedHashMap::<T, N> {
            data,
            hits: AtomicUsize::new(0),
            misses: AtomicUsize::new(0),
            conflicts: AtomicUsize::new(0),
            accepted: AtomicUsize::new(0),
            rejected: AtomicUsize::new(0),
            updates: AtomicUsize::new(0),
        }
    }

    pub fn get(&self, k: u64) -> Option<T> {
        let pos: usize = k as usize % N;
        let entry = self.data[pos].data.load(Ordering::Relaxed);
        let header = (entry >> 64) as u64;
        if header == 0 {
            self.misses.fetch_add(1, Ordering::Relaxed);
            return None;
        }
        if header != k {
            self.conflicts.fetch_add(1, Ordering::Relaxed);
            return None;
        }
        self.hits.fetch_add(1, Ordering::Relaxed);
        Some(T::from_u64(entry as u64))
    }

    /// Store a new key value pair in the hashmap. `insert` will not overwrite an existing entry,
    /// and will return a failure if something already exists in the desired hashmap position,
    /// regardless of if the key matches.
    ///
    /// returns true if the pair was successfully stored in an empty cell else false.
    pub fn insert(&self, k: u64, v: T) -> bool {
        // We reserve values of 0 to indicate uninitialized cells. For our use of this hashmap
        // where we store PackedTTEntries which contain a move, these can never be 0. (Consider
        // that a move contains a from an to position, and h1 is the only position that's encoded
        // as 0. Since a move can't be both to and from h1, at least one of "to" or "from" must be
        // non zero).
        assert_ne!(v.to_u64(), 0);

        // Modulo is fine for now, our zorbrist keys are hopefully effectively random. Since we
        // bail immediately if the spot is taken rather than trying to find a new one, we're not
        // worried about bunching or needing a backup method.
        let pos: usize = k as usize % N;
        let entry = &self.data[pos].data;

        // Rather than checking the key, when we insert, we just try to insert the value
        //
        // The trick we'll rely on is that we're only allowed to replace an unintialized value, and
        // the atomic cmpexchg will ensure that only one writer actually gets to write that value.
        let expected = ((k as u128) << 64) | v.to_u64() as u128;
        if let Err(_) = entry.compare_exchange(0, expected, Ordering::Relaxed, Ordering::Relaxed) {
            self.rejected.fetch_add(1, Ordering::Relaxed);
            false
        } else {
            self.accepted.fetch_add(1, Ordering::Relaxed);
            true
        }
    }

    /// Update an existing key in the hashmap from a known existing value to a new one.
    ///
    /// Returns the value previously contained in the hashmap. On a success, this will be equal to
    /// `from`.
    pub fn update(&self, k: u64, from: T, to: T) -> Result<T, T> {
        let pos: usize = k as usize % N;

        // We don't really care about the ordering on success or failure here. The data will be
        // valid regardless of whether a reader gets the old or new value.
        let expected = ((k as u128) << 64) | from.to_u64() as u128;
        let desired = ((k as u128) << 64) | to.to_u64() as u128;
        match self.data[pos].data.compare_exchange(
            expected,
            desired,
            Ordering::Relaxed,
            Ordering::Relaxed,
        ) {
            Ok(v) => {
                self.updates.fetch_add(1, Ordering::Relaxed);
                Ok(T::from_u64(v as u64))
            }
            Err(v) => {
                self.rejected.fetch_add(1, Ordering::Relaxed);
                Err(T::from_u64(v as u64))
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
        let map: SharedHashMap<u64, 1024> = SharedHashMap::new();
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
        let map: SharedHashMap<u64, 1024> = SharedHashMap::new();
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
        let map: SharedHashMap<u64, 1024> = SharedHashMap::new();
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
        let map: Arc<SharedHashMap<u64, 10000>> = Arc::new(SharedHashMap::new());
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
