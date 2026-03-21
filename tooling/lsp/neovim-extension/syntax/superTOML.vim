" Vim syntax file for SuperTOML
" Language: SuperTOML (TOML with Superposition extensions)
" Maintainer: Superposition Team
" Last Change: 2025-01-16
" Version: 0.1.0
"
" Based on the TOML syntax file with enhancements for SuperTOML

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
    finish
endif

" Sync from the start of the file
syn sync minlines=500

" -----------------------------------------------------------------------------
" Comments
" -----------------------------------------------------------------------------
syn match tomlComment "#.*$" contains=@Spell,tomlTodo

" TODO/FIXME/XXX highlighting in comments
syn keyword tomlTodo contained TODO FIXME XXX NOTE HACK

" -----------------------------------------------------------------------------
" Tables (Sections)
" -----------------------------------------------------------------------------
" Standard tables [table]
syn match tomlTable /^\s*\[.\+\]\s*$/ contains=tomlTableBrackets,tomlTableName

" Array of tables [[table]]
syn match tomlTableArray /^\s*\[\[.\+\]\]\s*$/ contains=tomlTableArrayBrackets,tomlTableName

" Dotted table names
syn match tomlTableName /[^[\]]\+/ contained contains=tomlDotSeparator

" Brackets
syn match tomlTableBrackets /\[\|\]/ contained
syn match tomlTableArrayBrackets /\[\[\|\]\]/ contained

" Dot separator in table names
syn match tomlDotSeparator /\./ contained

" -----------------------------------------------------------------------------
" Keys
" -----------------------------------------------------------------------------
" Bare keys (alphanumeric, dash, underscore)
syn match tomlKey /^\s*[a-zA-Z0-9_-]\+\s*=/me=e-1 contains=tomlKeyBare
syn match tomlKeyBare /[a-zA-Z0-9_-]\+/ contained

" Quoted keys
syn match tomlKeyQuoted /^\s*"[^"]\+"\s*=/me=e-1 contains=tomlString
syn match tomlKeyQuoted /^\s*'[^']\+'\s*=/me=e-1 contains=tomlLiteralString

" Dotted keys
syn match tomlDottedKey /^\s*[a-zA-Z0-9_-]\+\(\.[a-zA-Z0-9_-]\+\)*\s*=/me=e-1 contains=tomlKeyBare,tomlDotSeparator

" -----------------------------------------------------------------------------
" Strings
" -----------------------------------------------------------------------------
" Basic strings "..."
syn region tomlString start=/"/ skip=/\\\\\|\\"/ end=/"/ oneline contains=tomlEscape

" Multi-line basic strings """..."""
syn region tomlMultilineString start=/"""/ end=/"""/ contains=tomlEscape

" Literal strings '...'
syn region tomlLiteralString start=/'/ end=/'/ oneline

" Multi-line literal strings '''...'''
syn region tomlMultilineLiteralString start=/'''/ end=/'''/

" Escape sequences
syn match tomlEscape /\\[btnfr"\\]/ contained
syn match tomlEscape /\\u\x\{4}/ contained
syn match tomlEscape /\\U\x\{8}/ contained

" -----------------------------------------------------------------------------
" Numbers
" -----------------------------------------------------------------------------
" Integers (decimal)
syn match tomlInteger /\<[+-]\?\d\+\%(_\d\+\)*\>/

" Integers (hexadecimal)
syn match tomlHexInteger /\<0x[0-9A-Fa-f]\+\%(_[0-9A-Fa-f]\+\)*\>/

" Integers (octal)
syn match tomlOctInteger /\<0o[0-7]\+\%(_[0-7]\+\)*\>/

" Integers (binary)
syn match tomlBinInteger /\<0b[01]\+\%(_[01]\+\)*\>/

" Floats
syn match tomlFloat /\<[+-]\?\d\+\%(_\d\+\)*\%(\.\d\+\%(_\d\+\)*\)\?\%([eE][+-]\?\d\+\%(_\d\+\)*\)\?\>/
syn match tomlFloat /\<[+-]\?\.\d\+\%(_\d\+\)*\%([eE][+-]\?\d\+\%(_\d\+\)*\)\?\>/
syn match tomlFloat /\<[+-]\?\d\+\%(_\d\+\)*[eE][+-]\?\d\+\%(_\d\+\)*\>/

" Special floats
syn match tomlSpecialFloat /\<[+-]\?\%(inf\|nan\)\>/

" -----------------------------------------------------------------------------
" Booleans
" -----------------------------------------------------------------------------
syn keyword tomlBoolean true false

" -----------------------------------------------------------------------------
" Dates and Times
" -----------------------------------------------------------------------------
" Offset Date-Time
syn match tomlOffsetDateTime /\d\{4\}-\d\{2\}-\d\{2\}[T ]\d\{2\}:\d\{2\}:\d\{2\}\%(\.\d\+\)\?\%(Z\|[+-]\d\{2\}:\d\{2\}\)/

" Local Date-Time
syn match tomlLocalDateTime /\d\{4\}-\d\{2\}-\d\{2\}[T ]\d\{2\}:\d\{2\}:\d\{2\}\%(\.\d\+\)\?/

" Local Date
syn match tomlLocalDate /\d\{4\}-\d\{2\}-\d\{2\}/

" Local Time
syn match tomlLocalTime /\d\{2\}:\d\{2\}:\d\{2\}\%(\.\d\+\)\?/

" -----------------------------------------------------------------------------
" Arrays
" -----------------------------------------------------------------------------
syn region tomlArray start=/\[/ end=/\]/ transparent contains=tomlArrayPunctuation,tomlString,tomlMultilineString,tomlLiteralString,tomlMultilineLiteralString,tomlInteger,tomlHexInteger,tomlOctInteger,tomlBinInteger,tomlFloat,tomlSpecialFloat,tomlBoolean,tomlOffsetDateTime,tomlLocalDateTime,tomlLocalDate,tomlLocalTime,tomlArray,tomlInlineTable,tomlComment

syn match tomlArrayPunctuation /[\[\],]/ contained

" -----------------------------------------------------------------------------
" Inline Tables
" -----------------------------------------------------------------------------
syn region tomlInlineTable start=/{/ end=/}/ transparent contains=tomlInlineTablePunctuation,tomlKeyBare,tomlString,tomlLiteralString,tomlInteger,tomlHexInteger,tomlOctInteger,tomlBinInteger,tomlFloat,tomlSpecialFloat,tomlBoolean,tomlOffsetDateTime,tomlLocalDateTime,tomlLocalDate,tomlLocalTime,tomlArray,tomlInlineTable

syn match tomlInlineTablePunctuation /[{},=]/ contained

" -----------------------------------------------------------------------------
" Operators and Punctuation
" -----------------------------------------------------------------------------
syn match tomlOperator /=/
syn match tomlDelimiter /\./
syn match tomlComma /,/

" -----------------------------------------------------------------------------
" SuperTOML-specific Extensions
" -----------------------------------------------------------------------------
" Variable references ${var}
syn match tomlVariableReference /\${[^}]\+}/ contains=tomlVariableBraces,tomlVariableName
syn match tomlVariableBraces /[${}]/ contained
syn match tomlVariableName /[^${}]\+/ contained

" Environment variables $VAR or ${VAR}
syn match tomlEnvVar /\$[A-Za-z_][A-Za-z0-9_]*/
syn region tomlEnvVarBraced start=/\${/ end=/}/ contains=tomlEnvVarName
syn match tomlEnvVarName /[^}]\+/ contained

" Function calls (for computed values)
syn match tomlFunctionCall /\$\w\+(.\+)/ contains=tomlFunctionName,tomlFunctionArgs
syn match tomlFunctionName /\$\w\+/ contained
syn match tomlFunctionArgs /(.\+)/ contained

" Expression interpolation
syn region tomlExpression start=/\$(/ end=/)/ contains=tomlExpressionContent
syn match tomlExpressionContent /[^()]*/ contained

" -----------------------------------------------------------------------------
" Highlight Links
" -----------------------------------------------------------------------------
hi def link tomlComment Comment
hi def link tomlTodo Todo

hi def link tomlTable Structure
hi def link tomlTableArray Structure
hi def link tomlTableName Type
hi def link tomlTableBrackets Special
hi def link tomlTableArrayBrackets Special

hi def link tomlKey Identifier
hi def link tomlKeyBare Identifier
hi def link tomlDotSeparator Operator
hi def link tomlDelimiter Operator

hi def link tomlString String
hi def link tomlMultilineString String
hi def link tomlLiteralString String
hi def link tomlMultilineLiteralString String
hi def link tomlEscape SpecialChar

hi def link tomlInteger Number
hi def link tomlHexInteger Number
hi def link tomlOctInteger Number
hi def link tomlBinInteger Number
hi def link tomlFloat Float
hi def link tomlSpecialFloat Float

hi def link tomlBoolean Boolean

hi def link tomlOffsetDateTime Constant
hi def link tomlLocalDateTime Constant
hi def link tomlLocalDate Constant
hi def link tomlLocalTime Constant

hi def link tomlOperator Operator
hi def link tomlComma Delimiter
hi def link tomlArrayPunctuation Delimiter
hi def link tomlInlineTablePunctuation Delimiter

" SuperTOML-specific highlights
hi def link tomlVariableReference Special
hi def link tomlVariableBraces Special
hi def link tomlVariableName Identifier
hi def link tomlEnvVar PreProc
hi def link tomlEnvVarBraced PreProc
hi def link tomlEnvVarName PreProc
hi def link tomlFunctionCall Function
hi def link tomlFunctionName Function
hi def link tomlFunctionArgs String
hi def link tomlExpression Special
hi def link tomlExpressionContent Special

let b:current_syntax = "superTOML"
