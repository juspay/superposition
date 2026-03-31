;; Textobjects for SuperTOML (TOML with Superposition extensions)
;; These queries define text objects for selection and navigation

;; Key-value pairs
(pair
  key: (_) @assignment.inner
  value: (_) @assignment.inner) @assignment.outer

;; Keys
(pair
  key: (_) @key.inner) @key.outer

;; Values
(pair
  value: (_) @value.inner) @value.outer

;; Tables (sections)
(table
  (bare_key) @class.name
  (pair)* @class.inner) @class.outer

(table
  (dotted_key) @class.name
  (pair)* @class.inner) @class.outer

;; Array tables
(table_array_element
  (bare_key) @class.name
  (pair)* @class.inner) @class.outer

(table_array_element
  (dotted_key) @class.name
  (pair)* @class.inner) @class.outer

;; Arrays
(array
  (_) @parameter.inner) @parameter.outer

(array) @class.outer

;; Inline tables
(inline_table
  (_) @parameter.inner) @parameter.outer

;; Strings
(string) @class.outer
(multi_line_string) @class.outer
(literal_string) @class.outer
(multi_line_literal_string) @class.outer

;; Comments
(comment) @comment.outer

;; Numbers and booleans
(integer) @number.inner
(float) @number.inner
(boolean) @boolean.inner

;; Dates
(local_date) @date.inner
(local_time) @date.inner
(local_date_time) @date.inner
(offset_date_time) @date.inner
