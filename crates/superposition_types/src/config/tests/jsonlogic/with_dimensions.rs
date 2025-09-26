use serde_json::{from_value, json};

use crate::Config;

pub(crate) fn get_config() -> Config {
    let config_json = json!({
        "contexts": [
            {
                "id": "40c2564c114e1a2036bc6ce0e730289d05e117b051f2d286d6e7c68960f3bc7d",
                "condition": {
                    "==": [
                        {
                            "var": "test3"
                        },
                        true
                    ]
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
                    "==": [
                        {
                            "var": "test2"
                        },
                        123
                    ]
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
                    "and": [
                        {
                            "==": [
                                {
                                    "var": "test3"
                                },
                                false
                            ]
                        },
                        {
                            "==": [
                                {
                                    "var": "test"
                                },
                                "test"
                            ]
                        }
                    ]
                },
                "priority": 2,
                "weight": 2,
                "override_with_keys": [
                    "e2fa5b38c3a1448cf0e27f9d555fdb8964a686d8ae41b70b55e6ee30359b87c8"
                ]
            }
        ],
        "overrides": {
            "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef": {
                "test.test1": 5,
                "test2.test": "testval"
            },
            "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9": {
                "test2.key": true,
                "test2.test": "value"
            },
            "e2fa5b38c3a1448cf0e27f9d555fdb8964a686d8ae41b70b55e6ee30359b87c8": {
                "key1": true
            }
        },
        "default_configs": {
            "key1": false,
            "test.test.test1": 1,
            "test.test1": 12,
            "test2.key": false,
            "test2.test": "def_val"
        },
        "dimensions" : {
            "test3": {
                "schema": {
                    "type": "boolean"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 3,
                "dependency_graph": {}
            },
            "test2": {
                "schema": {
                    "type": "integer"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 2,
                "dependency_graph": {}
            },
            "test": {
                "schema": {
                    "pattern": ".*",
                    "type" : "string"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 1,
                "dependency_graph": {}
            }
        }
    });

    from_value(config_json).unwrap()
}

pub(crate) fn get_dimension_filtered_config1() -> Config {
    let config_json = json!({
        "contexts": [
            {
                "id": "40c2564c114e1a2036bc6ce0e730289d05e117b051f2d286d6e7c68960f3bc7d",
                "condition": {
                    "==": [
                        {
                            "var": "test3"
                        },
                        true
                    ]
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
                    "==": [
                        {
                            "var": "test2"
                        },
                        123
                    ]
                },
                "priority": 1,
                "weight": 1,
                "override_with_keys": [
                    "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9"
                ]
            }
        ],
        "overrides": {
            "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9": {
                "test2.key": true,
                "test2.test": "value"
            },
            "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef": {
                "test.test1": 5,
                "test2.test": "testval"
            }
        },
        "default_configs": {
            "key1": false,
            "test.test.test1": 1,
            "test.test1": 12,
            "test2.key": false,
            "test2.test": "def_val"
        },
        "dimensions" : {
            "test3": {
                "schema": {
                    "type": "boolean"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 3,
                "dependency_graph": {}
            },
            "test2": {
                "schema": {
                    "type": "integer"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 2,
                "dependency_graph": {}
            },
            "test": {
                "schema": {
                    "pattern": ".*",
                    "type" : "string"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 1,
                "dependency_graph": {}
            }
        }
    });

    from_value(config_json).unwrap()
}

pub(crate) fn get_dimension_filtered_config2() -> Config {
    let config_json = json!({
        "contexts": [
            {
                "id": "691ed369369ac3facdd07e5dd388e07ed682a7e212a04b7bcd0186e6f2d0d097",
                "condition": {
                    "==": [
                        {
                            "var": "test2"
                        },
                        123
                    ]
                },
                "priority": 1,
                "weight": 1,
                "override_with_keys": [
                    "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9"
                ]
            }
        ],
        "overrides": {
            "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9": {
                "test2.key": true,
                "test2.test": "value"
            }
        },
        "default_configs": {
            "key1": false,
            "test.test.test1": 1,
            "test.test1": 12,
            "test2.key": false,
            "test2.test": "def_val"
        },
        "dimensions" : {
            "test3": {
                "schema": {
                    "type": "boolean"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 3,
                "dependency_graph": {}
            },
            "test2": {
                "schema": {
                    "type": "integer"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 2,
                "dependency_graph": {}
            },
            "test": {
                "schema": {
                    "pattern": ".*",
                    "type" : "string"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 1,
                "dependency_graph": {}
            }
        }
    });

    from_value(config_json).unwrap()
}

pub(crate) fn get_prefix_filtered_config1() -> Config {
    let config_json = json!({
        "contexts": [
            {
                "id": "40c2564c114e1a2036bc6ce0e730289d05e117b051f2d286d6e7c68960f3bc7d",
                "condition": {
                    "==": [
                        {
                            "var": "test3"
                        },
                        true
                    ]
                },
                "priority": 0,
                "weight": 0,
                "override_with_keys": [
                    "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef"
                ]
            }
        ],
        "overrides": {
            "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef": {
                "test.test1": 5
            }
        },
        "default_configs": {
            "test.test.test1": 1,
            "test.test1": 12
        },
        "dimensions" : {
            "test3": {
                "schema": {
                    "type": "boolean"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 3,
                "dependency_graph": {}
            },
            "test2": {
                "schema": {
                    "type": "integer"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 2,
                "dependency_graph": {}
            },
            "test": {
                "schema": {
                    "pattern": ".*",
                    "type" : "string"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 1,
                "dependency_graph": {}
            }
        }
    });
    from_value(config_json).unwrap()
}

pub(crate) fn get_prefix_filtered_config2() -> Config {
    let config_json = json!({
        "contexts": [
            {
                "id": "40c2564c114e1a2036bc6ce0e730289d05e117b051f2d286d6e7c68960f3bc7d",
                "condition": {
                    "==": [
                        {
                            "var": "test3"
                        },
                        true
                    ]
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
                    "==": [
                        {
                            "var": "test2"
                        },
                        123
                    ]
                },
                "priority": 1,
                "weight": 1,
                "override_with_keys": [
                    "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9"
                ]
            }
        ],
        "overrides": {
            "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9": {
                "test2.key": true,
                "test2.test": "value"
            },
            "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef": {
                "test.test1": 5,
                "test2.test": "testval"
            }
        },
        "default_configs": {
            "test.test.test1": 1,
            "test.test1": 12,
            "test2.key": false,
            "test2.test": "def_val"
        },
        "dimensions" : {
            "test3": {
                "schema": {
                    "type": "boolean"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 3,
                "dependency_graph": {}
            },
            "test2": {
                "schema": {
                    "type": "integer"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 2,
                "dependency_graph": {}
            },
            "test": {
                "schema": {
                    "pattern": ".*",
                    "type" : "string"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 1,
                "dependency_graph": {}
            }
        }
    });
    from_value(config_json).unwrap()
}
