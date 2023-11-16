use derive_more::{Deref, DerefMut};
use std::vec::Vec;

#[derive(Clone, Debug)]
pub struct AppRoute {
    pub key: String,
    pub path: String,
    pub icon: String,
    pub label: String,
}
