**How a JSON schema looks (the part which is important)?**
```
{
  "type": <string/array>
}
```
`type`, can either be a string or an array of strings

Allowed values for `type`:
- string
- number
- integer
- object
- array
- boolean
- null

**SchemaType enum**
```rust
#[derive(Debug, Clone, strum_macros::EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum SchemaType {
    Boolean,
    Number,
    String,
    Integer,
    Array,
    Object,
    Null,
}
```

### Parsing (when type is known)

**Steps:**
- Get the type, there can be multiple types
- Try parsing for all possible types
- Type precedence 
    - boolean
    - integer
    - number
    - null
    - array
    - object
    - string
- In case of multiple types, the above mentioned order of parsing should be followed, whichever succeeds should be used.

**Implementation**
- `boolean, integer, number, null, string`: These can be directly parsed using 

    ``` 
     Value::<type>(given string) 
    ```

- `array, object`: 
    
    ```
    serde_json::from_str(given string)
    ```

### Impl for SchemaType enum
```rust
impl SchemaType {
    fn try_parse_from_string(&self, s: String) -> Result<serde_json::Value, String> {
        match self {
            Boolean => ...,
            Number => ...,
            String => ...,
            Integer => ...,
            Array => ...,
            Object => ...,
            Null => ...,
        }
    }
}
```
----

### Which operators we are supporting?

- ==
- <=
- in (only case where a list of values has to be provided)

Additional operators to be added:
- ===, we can replace == with this
- in, case similar to variantIds
    - In this case, operator can be named `has` 

**Operators:**
- ===
- <=
- in
- has

**Operator enum** (current version)
```rust
#[derive(Debug, Clone)]
pub enum Operator {
    Is,
    In,
    Has,
    Between,
    Other(String), // This can be removed and restrictions should be imposed on JSON logic expression
}
```

#### Bits on Json logic ()

`===`: Only two operands
```
{ "===": [ {"var": <dimension name>}, <value> ] }
```

`<=`: 3 operands
```
{ "<=": [ <value 1>, {"var": <dimension name>}, <value 2> ] }
```

`in`: 2 operands
```
{ "in": [ {"var": <dimension name>}, <array of values/a string> ] }
```

`has`: 2 operands
```
{ "in": [ <value>, {"var": <dimension name>} ] }
```

### Context Parsing (with operator as an additional parameter)

- Get the Operator and the possible types

- === / has:
    - Parse according to the type
- <=:
    - Split the input around a comma "," (after split only 2 values should be present)
    - Parse each of them according to the type
- in:
    - Cases:
        - A single value must be string
        - An array of values
    - Single Value:
        - Parse as a string
    - Multiple Values:
        - Parse each value according to the type

----

### JSON to Form input
```
format!("{}", json_value)
     or
json_value.to_string()
```
These should work.

----

### Form Trait

```rust
trait Form {
    type Error;
    type SchemaSpec; // JSON schema definitions
    type ParseOutput; // Output type after parse

    fn parse(&self, source: &[Self::SchemaSpec]) -> Result<Self::ParseOutput, Self::Error>;
    fn validate(&self) -> bool;
}
```

## Implementation Overview

Creating new wrapper types for forms we have
#### Forms Types
```rust
#[derive(Debug, Clone, Deref, DerefMut, Deserialize, Serialize)]
pub struct ContextForm(pub Vec<(String, Operator, String)>);

#[derive(Debug, Clone, Deref, DerefMut, Deserialize, Serialize)]
pub struct OverrideForm(pub Vec<(String, serde_json::Value)>);

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct VariantForm {
    pub overrides: OverrideForm,
    pub variant_type: VariantType,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ExperimentForm {
    pub name: String,
    pub context: ContextForm,
    pub variants: Vec<VariantForm>,
}
```

**NOTE: Implement `Form` trait for above mentioned types**

Implement `TryFrom` trait for above mentioned types 
    - `impl TryFrom<Context> for ContextForm`
    - `impl TryFrom<Override> for OverrideForm` (We don't have any concrete type for Override, Map<String, Value> is used)
    - `impl TryFrom<Variant> for VariantForm`
    - `impl TryFrom<Experiment> for ExperimentForm`

#### Context -> ContextForm

- Structure of JSON logic in Context
    ```js
    {
        "and": [
            {
                "<operator>": [
                    ....
                    // No nested conditions 
                ]
            },
            ....
        ]        
    }
            or    
    {
        "<operator>": [
            ....
            // No nested conditions 
        ]
    }
    ```

- Information required from above JSON for ContextForm
    - Operator, Dimension name, value

- Using how each operator dictates the data to be organized in JSON logic [mentioned above in Bits on JSON Logic] we can extract the required information i.e. operator, dimension name, dimension value 

NOTE: We can look into restricting conditions written using JSON logic

#### Override -> OverrideForm 
Trivial Map to Vector of key value entries conversion

*Variant -> VariantForm and Experiment -> ExperimentForm, will build on top of Context and Override coversions*

#### ContextForm's `parse`
- `SchemaSpec` will be `Dimension`, we can find the required dimension's type from this.
- `ParseOutput` can be `Vec<(String, Operator, serde_json::Value)>`
- Two parameters for parsing the values
    - Operator
    - SchemaType

Details[Context Parsing (with operator as an additional parameter)]

#### OverrideForm's `parse`
- `SchemaSpec` will be `DefaultConfig`, we can find the required config_key's type from this.
- `ParseOutput` can be `Vec<(String, serde_json::Value)>`

Details [Parsing (when type is known)]

#### VariantForm's `parse`
Reuses OverrideForm's parse

#### ExperimentForm's `parse`
Reuses ContextForm's and VariantForm's parse
