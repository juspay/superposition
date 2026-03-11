;; TreeSitter injections for SuperTOML
;; This file defines how other languages can be embedded in SuperTOML files

;; Inject shell for command values (common pattern)
((string) @injection.content
  (#match? @injection.content "^[\\s]*#!")
  (#set! injection.language "bash"))

;; Inject regex for pattern values
((string) @injection.content
  (#match? @injection.content "^/.*/$")
  (#set! injection.language "regex"))

;; Inject JSON for values that look like JSON
((string) @injection.content
  (#match? @injection.content "^\\s*[\\[{].*[\\]}]\\s*$")
  (#set! injection.language "json"))

;; Inject Lua for Neovim-style config values
((string) @injection.content
  (#match? @injection.content "^\\s*function\\s*\\(")
  (#set! injection.language "lua"))

;; Inject Lua for vimscript-style values
((string) @injection.content
  (#match? @injection.content "^\\s*vim\\.")
  (#set! injection.language "lua"))
