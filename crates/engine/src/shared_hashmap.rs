use std::cell::UnsafeCell;

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

#[derive(Debug, Copy, Clone)]
pub struct SharedHashMapEntry<T: Encodable + Copy + Clone> {
    key_xor_v: u64,
    value: T,
}

#[derive(Debug)]
pub struct SharedHashMapInner<T: Encodable + Copy + Clone, const N: usize> {
    data: UnsafeCell<[SharedHashMapEntry<T>; N]>,
}

#[derive(Debug)]
pub struct SharedHashMap<T: Encodable + Copy + Clone, const N: usize> {
    data: Box<SharedHashMapInner<T, N>>,
}

/// A very simple "lockless" hashmap that supports get/set. The map also supports updates, but only
/// updates of values, the key must remain the same. This allows us to update the evaluation of a
/// node if we compute the evaluation to a deeper later on due to reusing the cache between
/// iteration of iterative deepening.
///
/// In the pursuit of speed over correctness, we don't use atomics at all. Instead, we store the
/// value along side the key xor'd with the value. If the key we're looking up doesn't xor
/// correctly, we return that it's not found.
///
/// Keys are stored next to the values they are associated with using modulo to find a spot. If the
/// spot is already taken the insert will fail.
///
/// Really though, this is just 4 hashmaps, and we try to search/find from each one in turn,
/// starting from the smallest to the largest.
unsafe impl<T: Encodable + Copy + Clone, const N: usize> Send for SharedHashMap<T, N> {}
unsafe impl<T: Encodable + Copy + Clone, const N: usize> Sync for SharedHashMap<T, N> {}
impl<T: Encodable + Copy + Clone, const N: usize> Default for SharedHashMap<T, N> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Encodable + Copy + Clone, const N: usize> SharedHashMap<T, N> {
    pub fn new() -> SharedHashMap<T, N> {
        // We use the nightly "new_zeroed" because doing a regular box new causes a stack overflow
        // on non release builds.
        let data = Box::<SharedHashMapInner<T, N>>::new_zeroed();
        let data = unsafe { data.assume_init() };

        SharedHashMap::<T, N> { data }
    }

    pub fn clear(&self) {
        (&mut unsafe { *self.data.data.get() }).fill(SharedHashMapEntry {
            key_xor_v: 0,
            value: T::from_u64(0),
        })
    }

    // How deep we are able to search corresponds to how sparsely our hashmap will be filled, which
    // corresponds to how long we are able to search for. We don't know how long we are going to
    // search for a priori, this means we don't know how big our hashmap should be.
    //
    // Too big, and our use of modulo to place keys will randomly space them out through the, e.g.
    // 256MiB of memory. Every insert is going to eat overhead creating pages, then paging them
    // into cache.
    //
    // Too small, and we don't fit all the thing we want to cache.
    //
    // Instead, we attempt to somewhat naturally let the data choose the amount of pages it needs
    // by having a multi-level modulus attempting to somewhat densely pack entries into the front
    // of the map until it gets filled up.
    //
    // We have a form of n-hashing each entry can have several sections it can live, each section
    // larger than the last. If an entry isn't in the first section, we continue checking via the
    // later section hashes until it's found, or we find an empty key.
    const THRESHOLDS: [usize; 4] = [128 * 1024, 1024 * 1024, 16 * 1024 * 1024, N];
    const LEVELS: [usize; 4] = [
        if N > Self::THRESHOLDS[0] {
            Self::THRESHOLDS[0]
        } else {
            N
        },
        if N > Self::THRESHOLDS[1] {
            Self::THRESHOLDS[1]
        } else {
            N
        },
        if N > Self::THRESHOLDS[2] {
            Self::THRESHOLDS[2]
        } else {
            N
        },
        N,
    ];

    pub fn get(&self, k: u64) -> Option<T> {
        if k == 0 {
            return None;
        }

        let mut offset = 0;
        for section in Self::LEVELS {
            let pos = (k as usize + offset) % section;
            offset += 1;
            let key_xor_v = unsafe { &*self.data.data.get() }[pos].key_xor_v;
            let v = &unsafe { &*self.data.data.get() }[pos].value;
            let key = key_xor_v ^ v.to_u64();
            if key == 0 {
                return None;
            }
            if key != k {
                continue;
            }
            return Some(T::from_u64(v.to_u64()));
        }
        None
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
        debug_assert_ne!(v.to_u64(), 0);

        let mut offset = 0;
        for section in Self::LEVELS {
            let pos = (k as usize + offset) % section;
            offset += 1;
            let data = unsafe { &mut *self.data.data.get() };
            let exiting_key = &data[pos].key_xor_v;
            if *exiting_key != 0 {
                continue;
            }

            data[pos].key_xor_v = k ^ v.to_u64();
            data[pos].value = v;
            return true;
        }
        false
    }

    /// Update an existing key in the hashmap from a known existing value to a new one.
    ///
    /// Returns the value previously contained in the hashmap. On a success, this will be equal to
    /// `from`.
    pub fn update(&self, k: u64, from: T, to: T) -> Result<T, T> {
        let mut offset = 0;
        for section in Self::LEVELS {
            let pos = (k as usize + offset) % section;
            offset += 1;

            let data = unsafe { &mut *self.data.data.get() };
            let existing_value = &data[pos].value;
            let existing_keyxv = &data[pos].key_xor_v;
            let existing_key = existing_keyxv ^ existing_value.to_u64();
            if existing_key != k {
                continue;
            }
            if existing_value.to_u64() != from.to_u64() {
                return Err(*existing_value);
            }
            data[pos].key_xor_v = k ^ to.to_u64();
            data[pos].value = to;
            return Ok(from);
        }
        Err(T::from_u64(0))
    }
}

#[cfg(test)]
mod tests {
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
            let val = map.insert(i, 10000 - i);
            assert_eq!(val, true);
        }
        for i in 2..1026 {
            // Wrong from
            let val = map.update(i, 100 + i, 10000 + i);
            assert_eq!(val, Err(10000 - i));
        }
        for i in 2..1026 {
            // Doesn't exist
            let val = map.update(i + 2024, 100 + i, 10000 + i);
            assert_eq!(val, Err(0));
        }
    }

    #[test]
    fn update_same_key() {
        let map: SharedHashMap<u64, 1024> = SharedHashMap::new();
        for i in 1..1025 {
            let val = map.insert(i, 100000 - i);
            assert_eq!(val, true);
        }
        for i in 1..1025 {
            let val = map.update(i, 100000 - i, (100000 - i) + 1024);
            assert_eq!(val, Ok(100000 - i));
        }
        for i in 1..1025 {
            let val = map.get(i);
            assert_eq!(val, Some((100000 - i) + 1024));
        }
    }

    #[test]
    fn concurrent() {
        let map: SharedHashMap<u64, 10000> = SharedHashMap::new();
        rayon::scope(|s| {
            s.spawn(|_| {
                for i in 10000..20000 {
                    map.insert(i, i);
                }
            });
            s.spawn(|_| {
                for i in 20000..30000 {
                    map.insert(i, i);
                }
            });
            s.spawn(|_| {
                for i in 30000..40000 {
                    map.insert(i, i);
                }
            });
        });
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
