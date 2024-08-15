use std::cell::UnsafeCell;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};

pub struct SharedHashMapEntry<T> {
    key: AtomicU64,
    val: T,
}

pub struct SharedHashMap<T, const N: usize>
where
    T: Sized,
{
    // 1GiB
    data: UnsafeCell<Vec<SharedHashMapEntry<T>>>,
    hits: AtomicUsize,
    misses: AtomicUsize,
    conflicts: AtomicUsize,

    accepted: AtomicUsize,
    rejected: AtomicUsize,
}
unsafe impl<T, const N: usize> Send for SharedHashMap<T, N> {}
unsafe impl<T, const N: usize> Sync for SharedHashMap<T, N> {}
impl<T, const N: usize> SharedHashMap<T, N>
where
    T: Default,
{
    pub fn print_stats(&self) {
        let hits = self.hits.load(Ordering::Relaxed);
        let misses = self.misses.load(Ordering::Relaxed);
        let conflicts = self.conflicts.load(Ordering::Relaxed);
        let accepted = self.accepted.load(Ordering::Relaxed);
        let rejected = self.rejected.load(Ordering::Relaxed);
        println!("HashMap Stats: ");
        println!("Hits: {}", hits);
        println!("Misses: {}", misses);
        println!("Conflicts: {}", conflicts);
        println!(
            "Hit Rate: {}",
            hits as f64 / (hits + misses + conflicts) as f64
        );
        println!("");
        println!("Accepted: {}", accepted);
        println!("Rejected: {}", rejected);
        println!(
            "Acceptance Rate: {}",
            accepted as f64 / (accepted + rejected) as f64
        );
    }
    pub fn new() -> SharedHashMap<T, N> {
        let mut vec = Vec::with_capacity(N);
        for _ in 0..N {
            vec.push( SharedHashMapEntry {
                key: AtomicU64::new(0),
                val: T::default(),
            });
        }
        SharedHashMap {
            data: UnsafeCell::new(vec),
            hits: AtomicUsize::new(0),
            misses: AtomicUsize::new(0),
            conflicts: AtomicUsize::new(0),
            accepted: AtomicUsize::new(0),
            rejected: AtomicUsize::new(0),
        }
    }
    pub fn get(&self, k: u64) -> Option<&T> {
        let pos: usize = k as usize % N;
        let kp = unsafe { &self.data.get().as_ref().unwrap()[pos].key };
        let v = unsafe { &self.data.get().as_ref().unwrap()[pos].val };
        let header = kp.load(Ordering::Acquire);
        if header == 0 {
            self.misses.fetch_add(1, Ordering::Relaxed);
            return None;
        }
        if header != k {
            self.conflicts.fetch_add(1, Ordering::Relaxed);
            return None;
        }
        self.hits.fetch_add(1, Ordering::Relaxed);
        Some(v)
    }

    pub fn insert(&self, k: u64, v: T) -> bool {
        let pos: usize = k as usize % N;
        let kp = unsafe { &self.data.get().as_ref().unwrap()[pos].key };

        let res = kp.compare_exchange(0, 1, Ordering::AcqRel, Ordering::Relaxed);
        match res {
            Ok(_) => {
                self.accepted.fetch_add(1, Ordering::Relaxed);
                unsafe {
                    let _ = &self
                        .data
                        .get()
                        .as_mut()
                        .map(|x: &mut Vec<SharedHashMapEntry<T>>| x[pos].val = v);
                };
                kp.store(k, Ordering::Release);
                true
            }
            Err(_) => {
                self.rejected.fetch_add(1, Ordering::Relaxed);
                false
            }
        }
    }
}
