use std::array;


#[derive(Debug)]
pub struct HashMap<T, const N: usize> {
    // 1GiB
    data: Box<[(u64, T); N]>
}
impl <T, const N: usize> HashMap<T, N>  where T: Default {
    pub fn new() -> HashMap<T, N> {
        HashMap {
            data: Box::new(array::from_fn(|_|(0, T::default()))),
        }
    }
    pub fn get(&self, k: u64) -> Option<&T> {
        let pos: usize = k as usize % N;
        let (kp,v) = &self.data[pos];
        if *kp != k {
            return None;
        }
        Some(v)
    }

    pub fn insert(&mut self, k: u64, v: T) -> bool {
        let pos: usize = k as usize % N;
        let (kp, _) = &self.data[pos];
        if *kp != 0 {
            return false;
        }
        self.data[pos] = (k, v);
        true
    }
}
