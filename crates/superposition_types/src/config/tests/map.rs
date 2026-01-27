pub(crate) mod with_dimensions;
pub(crate) mod without_dimensions;

use serde_json::{from_value, json};

use crate::Context;

pub(crate) fn get_dimension_filtered_contexts1() -> Vec<Context> {
    let contexts = json!([
        {
            "id": "40c2564c114e1a2036bc6ce0e730289d05e117b051f2d286d6e7c68960f3bc7d",
            "condition": {
                 "test3": true
            },
            "priority": 0,
            "weight": 0,
            "override_with_keys": [
                "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef"
            ]
        },
        {
            "id": "9fbf3b9fa10caaaf31f6003cbd20ed36d40efe73b5c6b238288c0a96e6933500",
            "condition": {
                "test3": false,
                "test": "test"
            },
            "priority": 2,
            "weight": 2,
            "override_with_keys": [
                "e2fa5b38c3a1448cf0e27f9d555fdb8964a686d8ae41b70b55e6ee30359b87c8"
            ]
        }
    ]);

    from_value(contexts).unwrap()
}

pub(crate) fn get_dimension_filtered_contexts2() -> Vec<Context> {
    let contexts = json!([
        {
            "id": "40c2564c114e1a2036bc6ce0e730289d05e117b051f2d286d6e7c68960f3bc7d",
            "condition": {
                "test3": true
            },
            "priority": 0,
            "weight": 0,
            "override_with_keys": [
                "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef"
            ]
        },
        {
            "id": "691ed369369ac3facdd07e5dd388e07ed682a7e212a04b7bcd0186e6f2d0d097",
            "condition": {
                "test2": 123
            },
            "priority": 1,
            "weight": 1,
            "override_with_keys": [
                "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9"
            ]
        },
        {
            "id": "9fbf3b9fa10caaaf31f6003cbd20ed36d40efe73b5c6b238288c0a96e6933500",
            "condition": {
                "test3": false,
                "test": "test"
            },
            "priority": 2,
            "weight": 2,
            "override_with_keys": [
                "e2fa5b38c3a1448cf0e27f9d555fdb8964a686d8ae41b70b55e6ee30359b87c8"
            ]
        }
    ]);

    from_value(contexts).unwrap()
}
