use serde::{Deserialize, Serialize};

pub mod casbin;

#[derive(Eq, PartialEq, Hash, Clone, Debug, Serialize, Deserialize, PartialOrd, Ord)]
pub enum ResourceActionType {
    Action(String),
    Group(String),
}

impl ResourceActionType {
    pub fn get_name(&self) -> &str {
        match self {
            ResourceActionType::Action(name) | ResourceActionType::Group(name) => name,
        }
    }
}
