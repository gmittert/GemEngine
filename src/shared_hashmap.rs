use std::sync::atomic::{fence, AtomicU64, AtomicUsize, Ordering};

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
    pub fn clear(&self) {
        for i in 0..N {
            self.data[i].key.store(0, Ordering::Relaxed)
        }
        fence(Ordering::Release)
    }
    pub fn get(&self, k: u64) -> Option<u64> {
        if k == 1 || k == 0 {
            return None;
        }
        let pos: usize = k as usize % N;
        let key = &self.data[pos].key;
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
        let v = *&self.data[pos].val.load(Ordering::Acquire);
        Some(v)
    }

    pub fn insert(&self, k: u64, v: u64) -> bool {
        if k == 1 || k == 0 {
            return false;
        }
        let pos: usize = k as usize % N;
        let key = &self.data[pos].key;
        let expected = key.load(Ordering::Acquire);

        if expected != 0 && expected != k {
            self.rejected.fetch_add(1, Ordering::Relaxed);
            return false;
        }
        if expected == k {
            self.updates.fetch_add(1, Ordering::Relaxed);
        }

        let res = key.compare_exchange(expected, 1, Ordering::AcqRel, Ordering::Relaxed);
        match res {
            Ok(_) => {
                self.accepted.fetch_add(1, Ordering::Relaxed);
                self.data[pos].val.store(v, Ordering::Release);
                key.store(k, Ordering::Release);
                true
            }
            Err(_) => {
                self.rejected.fetch_add(1, Ordering::Relaxed);
                false
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
            let val = map.insert(i, i + 1024);
            assert_eq!(val, true);
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
