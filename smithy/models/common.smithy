$version: "2.0"

namespace io.superposition

map Condition {
    key: String
    value: Document
}

map Overrides {
    key: String
    value: Document
}

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

string Weight

enum SortBy {
    Desc = "desc"
    Asc = "asc"
}