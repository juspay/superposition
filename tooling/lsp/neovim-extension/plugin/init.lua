-- SuperTOML Analyzer - Neovim Plugin
-- This file is automatically loaded by Neovim when the plugin is initialized

-- Only load once
if vim.g.loaded_supertoml_analyzer then
    return
end
vim.g.loaded_supertoml_analyzer = 1

-- Save compatibility options
local save_cpo = vim.o.cpoptions
vim.o.cpoptions = ""

-- Check for Neovim version compatibility
if vim.fn.has("nvim-0.9") == 0 then
    vim.notify(
        "[supertoml-analyzer] This plugin requires Neovim 0.9 or later.",
        vim.log.levels.ERROR
    )
    return
end

-- Define global commands that are always available
vim.api.nvim_create_user_command("SuperTOMLSetup", function(opts)
    local supertoml = require("supertoml-analyzer")

    -- Parse optional config from command arguments
    local config = {}
    if opts.args and opts.args ~= "" then
        local ok, parsed = pcall(vim.fn.eval, opts.args)
        if ok and type(parsed) == "table" then
            config = parsed
        end
    end

    supertoml.setup(config)
end, {
    nargs = "?",
    complete = function(_, cmdline, _)
        -- Simple completion for common options
        local options = {
            "server={}",
            "server={path=''}",
            "diagnostics={enable=true}",
            "diagnostics={enable=false}",
            "completions={enable=true}",
            "completions={enable=false}",
            "hover={enable=true}",
            "hover={enable=false}",
            "log_level='debug'",
            "log_level='info'",
            "log_level='warn'",
            "log_level='error'",
        }
        return options
    end,
    desc = "Setup SuperTOML analyzer with optional configuration",
})

-- Provide a global variable to check if plugin is loaded
vim.g.supertoml_analyzer_loaded = 1

-- Auto-setup if configured
local function auto_setup()
    -- Check if auto-setup is disabled
    if vim.g.supertoml_analyzer_auto_setup == false then
        return
    end

    -- Check if there's a setup call in the user's config
    -- If not, auto-setup with defaults
    local supertoml = require("supertoml-analyzer")

    -- Check for lazy.nvim integration
    local lazy_config = vim.g.supertoml_analyzer_config
    if lazy_config then
        supertoml.setup(lazy_config)
    else
        -- Check if the user has configured the server path via vim.g
        local server_path = vim.g.supertoml_analyzer_server_path
        local config = {}

        if server_path then
            config.server = { path = server_path }
        end

        -- Only auto-setup if explicitly enabled or if we have SuperTOML files
        local auto_setup_enabled = vim.g.supertoml_analyzer_auto_setup
        if auto_setup_enabled == true then
            supertoml.setup(config)
        end
    end
end

-- Schedule auto-setup to run after VimEnter
-- This allows user configs to load first
vim.api.nvim_create_autocmd("VimEnter", {
    group = vim.api.nvim_create_augroup("SuperTOMLAutoSetup", { clear = true }),
    callback = auto_setup,
    once = true,
})

-- Also provide immediate setup for FileType events if a SuperTOML file is opened
vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("SuperTOMLLazySetup", { clear = true }),
    pattern = "superTOML",
    callback = function()
        -- Only setup if not already done
        if not vim.g.supertoml_analyzer_setup_done then
            vim.g.supertoml_analyzer_setup_done = 1
            auto_setup()
        end
    end,
    once = true,
})

-- Restore compatibility options
vim.o.cpoptions = save_cpo
