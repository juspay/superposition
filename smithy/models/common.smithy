$version: "2.0"

namespace io.superposition

@documentation("Represents conditional criteria used for context matching. Keys define dimension names and values specify the criteria that must be met.")
map Condition {
    key: String
    value: Document
}

@documentation("Configuration overrides that replace default values when context conditions are met. Keys represent configuration keys and values are the override data.")
map Overrides {
    key: String
    value: Document
}

@documentation("Generic key-value object structure used for flexible data representation throughout the API.")
map Object {
    key: String
    value: Document
}

/// Map representing the context.
/// Keys correspond to the names of the dimensions.
map ContextMap {
    key: String
    value: Document
}

list StringList {
    member: String
}

@documentation("Priority weight used to determine the order of context evaluation. Higher weights take precedence during configuration resolution.")
string Weight

@documentation("Sort order enumeration for list operations.")
enum SortBy {
    @documentation("Descending order (Z-A, newest first)")
    Desc = "desc"
    @documentation("Ascending order (A-Z, oldest first)")
    Asc = "asc"
}