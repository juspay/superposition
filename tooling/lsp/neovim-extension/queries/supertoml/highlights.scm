; TreeSitter highlights for SuperTOML
; Based on tree-sitter-toml grammar

; Comments
(comment) @comment @spell

; Table headers
(table
  (bare_key) @namespace
  (dotted_key) @namespace)

(table
  "["
  (bare_key) @namespace
  (dotted_key) @namespace
  "]") @punctuation.bracket

; Array of tables
(table_array_element
  "[[" @punctuation.bracket
  (bare_key) @namespace
  (dotted_key) @namespace
  "]]" @punctuation.bracket)

(table_array_element
  (bare_key) @namespace
  (dotted_key) @namespace)

; Keys
(key
  (bare_key) @property)

(key
  (dotted_key
    (bare_key) @property))

(key
  (dotted_key) @property)

; Strings
(string) @string
(multi_line_string) @string
(string_escape) @string.escape

; Literal strings
(literal_string) @string
(multi_line_literal_string) @string

; Numbers
(integer) @number
(float) @number.float
(hex_integer) @number.hex
(oct_integer) @number.oct
(bin_integer) @number.bin

; Special values
(boolean) @boolean
(local_date) @string.special
(local_date_time) @string.special
(local_time) @string.special
(offset_date_time) @string.special

; Punctuation
"," @punctuation.delimiter
"." @punctuation.delimiter
"=" @operator

; Brackets
"[" @punctuation.bracket
"]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket

; Inline table
(inline_table
  "{" @punctuation.bracket
  "}" @punctuation.bracket)

; Array
(array
  "[" @punctuation.bracket
  "]" @punctuation.bracket)

; Special keys that look like keywords
(key
  (bare_key) @keyword
  (#match? @keyword "^(true|false)$"))
