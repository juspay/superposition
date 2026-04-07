-- Filetype plugin for SuperTOML
-- This file is sourced when a buffer's filetype is set to 'superTOML'

-- Set comment string for comment.nvim and other plugins
vim.bo.commentstring = "# %s"

-- Set tab settings (TOML convention: 2 spaces)
vim.bo.tabstop = 2
vim.bo.shiftwidth = 2
vim.bo.expandtab = true
vim.bo.softtabstop = 2

-- Set indent behavior
vim.bo.autoindent = true
vim.bo.smartindent = true

-- Set text width (optional, can be disabled by setting to 0)
vim.bo.textwidth = 0

-- Enable folding based on markers or syntax
vim.bo.foldmethod = "syntax"
vim.bo.foldlevel = 99  -- Start with all folds open

-- Set format options
-- t: Auto-wrap text using textwidth
-- c: Auto-wrap comments using textwidth
-- r: Automatically insert comment leader after <Enter>
-- o: Automatically insert comment leader after 'o' or 'O'
-- q: Allow formatting of comments with 'gq'
-- j: Remove comment leader when joining lines
vim.bo.formatoptions = "tcroqj"

-- Set match pairs for bracket matching
vim.bo.matchpairs = "(:),{:},[:]"

-- Set local mappings for this filetype
local bufnr = vim.api.nvim_get_current_buf()
local opts = { buffer = bufnr, noremap = true, silent = true }

-- Quick insert of tables
vim.keymap.set("i", "<C-t>", "[]<Left>", vim.tbl_extend("keep", { desc = "Insert empty table" }, opts))

-- Quick insert of inline tables
vim.keymap.set("i", "<C-g>", "{}<Left>", vim.tbl_extend("keep", { desc = "Insert inline table" }, opts))

-- Quick insert of arrays
vim.keymap.set("i", "<C-a>", "[]<Left>", vim.tbl_extend("keep", { desc = "Insert array" }, opts))

-- Quick insert of strings
vim.keymap.set("i", '<C-s>', '""<Left>', vim.tbl_extend("keep", { desc = "Insert string" }, opts))

-- Navigation between tables
vim.keymap.set("n", "]t", function()
    vim.fn.search("^\\[.*\\]$", "w")
end, vim.tbl_extend("keep", { desc = "Go to next table" }, opts))

vim.keymap.set("n", "[t", function()
    vim.fn.search("^\\[.*\\]$", "wb")
end, vim.tbl_extend("keep", { desc = "Go to previous table" }, opts))

-- Navigation between array tables
vim.keymap.set("n", "]a", function()
    vim.fn.search("^\\[\\[.*\\]\\]$", "w")
end, vim.tbl_extend("keep", { desc = "Go to next array table" }, opts))

vim.keymap.set("n", "[a", function()
    vim.fn.search("^\\[\\[.*\\]\\]$", "wb")
end, vim.tbl_extend("keep", { desc = "Go to previous array table" }, opts))

-- Set up comment continuation for auto-pairs
vim.opt_local.comments = ":#"
vim.opt_local.commentstring = "# %s"

-- Set up omnifunc for completion (will be overridden by LSP if attached)
vim.bo.omnifunc = "v:lua.vim.lsp.omnifunc"

-- Set up indent keys for automatic indentation
vim.opt_local.indentkeys = "0{,0},0],0),!^F,o,O,e,0=."

-- Disable swap file for large TOML files (optional optimization)
local max_filesize = 100 * 1024  -- 100 KB
local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(bufnr))
if ok and stats and stats.size > max_filesize then
    vim.bo.swapfile = false
end
