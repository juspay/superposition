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

list StringList {
    member: String
}
