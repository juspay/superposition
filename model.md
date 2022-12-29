[dimensions]
e.g.
  cug
  sdk_version
  merchant


[global config]
k1: v1

[override]
{
  ref1: {
    k1: v2
  },
  ref2: {
    k1: v3
  },
  ref3: {
    k1: v4
  },
  ref4: {
    k1: v4
  }
}

[context]
{
  c1: {
    // condition
  },
  c2: {
    // condition
  },
  c3: {
    // condition
  },
  c4: {
    // condition
  }
}

[context-override]
{
  context-config1: { context: c1, ref: [ref1]},
  context-config2: { context: c2, ref: [ref2]},
  context-config3: { context: c3, ref: [ref3]},
  context-config4: { context: c4, ref: [ref4]},
}

The above samples show the 5 different type of objects that the new config
system is based on.  The sample APIs below cover what these objects represent.

# Primitive Config APIs

  0. dimension APIs (TBD)
    - create new dimension
      name, type, priority
    - update dimension
    - get all dimension

  1. create new global config (key, value) - POST /context-aware-config/v1/config
    - create a new key/value to global config

  2. create a override - POST /context-aware-config/v1/override
    - override - is a set of key/value pairs where the key is a member of the global config keys

  3. create a context - POST /context-aware-config/v1/context
    - context - is a set of conditions involving dimensions (typically AND)
      [
        json-logic for m1,
        json-logic for sdk_version,
        json-logic for toss
      ]

  4. create a context-override - POST /context-aware-config/v1/context-override
    - context-override - captures the override that should apply when the context is matched

  5. delete context-override (context-override-id) - DEL /context-aware-config/v1/context-override/{context-override-id}

  6. delete key from global-config

# Derived Config APIs

  1. add context-override (context, override) - POST /context-aware-config/v1/context-override (overloaded version of 4 in previous section)
    - create a override
    - create a context
    - create a context-override

  co0: c0, o0
    c0: m1=zee5
    o0: hv=v0, k2=v5

  co1: c1, o1
    c1: m1=zee5,toss<=90
    o1: hv=v1

    // after reduce
    c0: m1=zee5
    if c0 exists
      merge o1 with (select override from context-override where context = c0) and created a new override as o3
      create new context-override (c0, o3)
    else
      create new context-override (c0, o1)

  2. reduce context-override (context-override-id, sub-context to removed) - POST /context-aware-config/v1/context-override/{context-override-id}/reduce
    - create a new context payload by removing context to be removed
    - if resultant context exists already - merge overrides with existing overrides to create new payload
    - create a new context-config after dropping sub-context
    - delete existing context-config

  co1:
    c1: [m=m1]
    o1:
      hyperpay_version: v1
      some_other_key: r2

    [m=m1, toss<=80] c2/o2/co2
      hyperpay_version: v2

# Fetch Config APIs

  1. GET /context-aware-config/v1/global-config/?filter=merchant:m1,platform:ios,tier:t1
    - get all configs application (global-config, overrides, context-overrides)
    - support filter to filter contexts that satisfy particular condition

    platform:1
    merchant:4
    tier:2
    sdk_version:3

    platform=ios (1)
      platform=android
      platform=web
      m=meesho
    platform=ios,m=m1 (1+4)
    platform=ios,m=m1,sdk_version=5 (1+4+3)
    platform=ios,sdk_version=6 (1+3)
    m=m1
      m=m2
      m=m3
      m=m4
      platform=web,m=m1
    sdk_version=3 (3)
    tier=t1 (2)
      tier=t2
      tier=t3

    - support sort contexts (sortOrder=tier,merchant)
      - for e.g. for context-configs involving merchant filter, ensure merchant = m1

      [tier=t2]
        hyperpay: h1

# Config Release APIs

  1. New CUG (aka CUG)
    - user provides context-override (automatically add cug == true context)

  2. New Release (aka Stagger 0)
    - drop CUG=true context
    - add toss <= 0 context

  3. Ramp
    - drop previous ramp context-config
    - add toss <= ramp context-config

  4. Stabilize
    - reduce context-override (removing toss sub-context)

  5. Revert
    - drop the context-override


Data modelling

dimension: key | type | priority |
global config: key | value |
context: context-id | merchant | sdk_version | ...... | dimension-n |
override: override-id | key | value |
context-override: context-override-id | context-id | override-id |









# Output structure

-| Sample 1
{ "dimensions": [
    { condition : {

        "and" : [
            { "==" : [{"var": "tier"}, "1"],
            { "==" : [{"var": "mid"}, "zee5"]}
        ]
    }
  ]
}

-| Sample 2
{ "dimensions": [
      {
          "condition": {
              "==": [{"var":"tier"}, "1"]
          },
          "overrideWithKeys": ["tier1"]

      },
      {
          "condition": {
              "==": [{"var":"merchantId"}, "zee5"]
          },
          "overrideWithKeys": ["zee5"]
      }
  ]
}


# Input structure
{ "tier": { "==" : [{"var": "tier"}, "1"]},
  "mid": { "==" : [{"var": "mid"}, "zee5"]}
}

# New input structure
{ "tier":
    { "operator": "=="
    , "value" : "1"
    },
  "mid":
    { "operator": "=="
    , "value" : "zee5"
    },
}


# DB structure
tier=1&mid=zee5