
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher}
};

pub fn string_based_b64_hash<T>(obj: T) -> u64 where T: Hash, {
    let mut hasher = DefaultHasher::new();
    obj.hash(&mut hasher);
    hasher.finish()
}