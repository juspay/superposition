-- Filetype detection for SuperTOML files
-- This file is automatically loaded by Neovim from the ftdetect/ directory

-- Detect SuperTOML files by extension and filename
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
    pattern = { "*.super.toml", "*.stoml", "super.toml" },
    callback = function(args)
        local filename = vim.api.nvim_buf_get_name(args.buf)
        local basename = vim.fs.basename(filename)

        -- Check for .super.toml extension
        if filename:match("%.super%.toml$") then
            vim.api.nvim_buf_set_option(args.buf, "filetype", "superTOML")
            return
        end

        -- Check for .stoml extension
        if filename:match("%.stoml$") then
            vim.api.nvim_buf_set_option(args.buf, "filetype", "superTOML")
            return
        end

        -- Check for exact filename "super.toml"
        if basename == "super.toml" then
            vim.api.nvim_buf_set_option(args.buf, "filetype", "superTOML")
            return
        end
    end,
    group = vim.api.nvim_create_augroup("SuperTOMLFtDetect", { clear = true }),
})
