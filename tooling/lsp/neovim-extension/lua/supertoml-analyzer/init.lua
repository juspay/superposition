---@class SuperTOMLAnalyzer
---@field setup fun(opts: SuperTOMLConfig|nil) Setup the plugin with optional configuration
---@field start fun() Start the language server
---@field stop fun() Stop the language server
---@field restart fun() Restart the language server
---@field attach fun(bufnr: integer|nil) Attach LSP to a buffer
---@field status fun() Get server status
---@field is_running fun() Check if server is running
---@field config SuperTOMLConfigModule Configuration module
---@field lsp SuperTOMLLSPModule LSP module

---@class SuperTOMLConfigModule
---@field setup fun(opts: table) Setup configuration
---@field get fun() Get current configuration
---@field get_server_cmd fun() Get server command
---@field get_init_options fun() Get initialization options
---@field get_capabilities fun() Get LSP capabilities

---@class SuperTOMLLSPModule
---@field start fun(opts: table|nil) Start the LSP server
---@field stop fun() Stop the LSP server
---@field restart fun() Restart the LSP server
---@field attach fun(bufnr: integer|nil) Attach to a buffer
---@field attach_to_buffers fun() Attach to all open SuperTOML buffers
---@field is_running fun() Check if server is running
---@field status fun() Get server status
---@field setup_autocmds fun() Set up autocommands
---@field is_super_toml_buffer fun(bufnr: integer|nil) Check if buffer is SuperTOML
---@field get_filetype fun() Get the filetype string
---@field server_name string The server name

local config = require("supertoml-analyzer.config")
local lsp = require("supertoml-analyzer.lsp")

local M = {}

-- Re-export modules for advanced usage
M.config = config
M.lsp = lsp

--- Version of the plugin
M._VERSION = "0.1.0"

--- Setup the SuperTOML analyzer plugin
---@param opts SuperTOMLConfig|nil User configuration options
---@return SuperTOMLAnalyzer
function M.setup(opts)
    -- Setup configuration
    config.setup(opts)

    -- Set up autocommands for auto-detection and LSP startup
    lsp.setup_autocmds()

    -- Set up filetype detection
    M.setup_filetype_detection()

    -- Set up user commands
    M.setup_commands()

    -- Set up diagnostic signs (if not already set by another LSP)
    M.setup_diagnostic_signs()

    -- Set up diagnostic configuration
    M.setup_diagnostics()

    return M
end

--- Set up filetype detection for SuperTOML files
function M.setup_filetype_detection()
    -- This is also handled by ftdetect/ script, but we also set it here
    -- for cases where the plugin is loaded after BufRead
    vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
        group = vim.api.nvim_create_augroup("SuperTOMLFiletype", { clear = true }),
        pattern = { "*.super.toml", "*.stoml", "super.toml" },
        callback = function(args)
            local current_ft = vim.api.nvim_buf_get_option(args.buf, "filetype")
            if current_ft == "" or current_ft == "toml" then
                vim.api.nvim_buf_set_option(args.buf, "filetype", "superTOML")
            end
        end,
    })
end

--- Set up user commands
function M.setup_commands()
    -- Start server command
    vim.api.nvim_create_user_command("SuperTOMLStart", function()
        M.start()
    end, {
        desc = "Start the SuperTOML language server",
    })

    -- Stop server command
    vim.api.nvim_create_user_command("SuperTOMLStop", function()
        M.stop()
    end, {
        desc = "Stop the SuperTOML language server",
    })

    -- Restart server command
    vim.api.nvim_create_user_command("SuperTOMLRestart", function()
        M.restart()
    end, {
        desc = "Restart the SuperTOML language server",
    })

    -- Status command
    vim.api.nvim_create_user_command("SuperTOMLStatus", function()
        local status = M.status()
        local client_id = M.lsp.client_id or "none"
        vim.notify(
            string.format("[supertoml-analyzer] Status: %s (client_id: %s)", status, client_id),
            vim.log.levels.INFO
        )
    end, {
        desc = "Show SuperTOML language server status",
    })

    -- Attach to current buffer command
    vim.api.nvim_create_user_command("SuperTOMLAttach", function()
        M.attach(0)
    end, {
        desc = "Attach SuperTOML language server to current buffer",
    })

    -- Show log command
    vim.api.nvim_create_user_command("SuperTOMLLog", function()
        local log_path = vim.lsp.get_log_path()
        vim.cmd("tabnew " .. log_path)
    end, {
        desc = "Open the LSP log file",
    })

    -- Check health command
    vim.api.nvim_create_user_command("SuperTOMLCheck", function()
        M.check_health()
    end, {
        desc = "Check SuperTOML language server health",
    })
end

--- Set up diagnostic signs
function M.setup_diagnostic_signs()
    -- Only set up if not already configured
    local signs = { "Error", "Warn", "Hint", "Info" }
    for _, sign in ipairs(signs) do
        local name = "DiagnosticSign" .. sign
        if vim.fn.sign_getdefined(name).text == nil then
            vim.fn.sign_define(name, {
                text = M.get_diagnostic_sign_text(sign),
                texthl = name,
                numhl = "",
            })
        end
    end
end

--- Get diagnostic sign text for a sign type
---@param sign string The sign type (Error, Warn, Hint, Info)
---@return string
function M.get_diagnostic_sign_text(sign)
    local signs = {
        Error = "✘",
        Warn = "⚠",
        Hint = "➤",
        Info = "ℹ",
    }
    return signs[sign] or "●"
end

--- Set up diagnostic configuration
function M.setup_diagnostics()
    vim.diagnostic.config({
        virtual_text = {
            prefix = "●",
            spacing = 4,
        },
        signs = true,
        underline = true,
        update_in_insert = false,
        severity_sort = true,
        float = {
            border = "rounded",
            source = "always",
            header = "",
            prefix = "",
        },
    })
end

--- Start the language server
---@return integer|nil client_id
function M.start()
    return lsp.start()
end

--- Stop the language server
---@return boolean
function M.stop()
    return lsp.stop()
end

--- Restart the language server
---@return integer|nil client_id
function M.restart()
    return lsp.restart()
end

--- Attach the LSP to a buffer
---@param bufnr integer|nil Buffer number (default: current buffer)
---@return boolean
function M.attach(bufnr)
    return lsp.attach(bufnr)
end

--- Check if the language server is running
---@return boolean
function M.is_running()
    return lsp.is_running()
end

--- Get the language server status
---@return string
function M.status()
    return lsp.status()
end

--- Check the health of the SuperTOML setup
function M.check_health()
    local health = {
        ok = true,
        messages = {},
    }

    -- Check if binary exists
    local cmd = config.get_server_cmd()
    local binary = cmd[1]

    if vim.fn.executable(binary) == 1 then
        table.insert(health.messages, "✓ Binary found: " .. binary)
    else
        health.ok = false
        table.insert(health.messages, "✗ Binary not found: " .. binary)
        table.insert(health.messages, "  Build it with: cargo build -p supertoml_lsp")
    end

    -- Check if server is running
    if lsp.is_running() then
        table.insert(health.messages, "✓ Language server is running")
    else
        table.insert(health.messages, "○ Language server is not running")
    end

    -- Check configuration
    local cfg = config.get()
    table.insert(health.messages, "✓ Configuration loaded")
    table.insert(health.messages, "  - Diagnostics: " .. tostring(cfg.diagnostics.enable))
    table.insert(health.messages, "  - Completions: " .. tostring(cfg.completions.enable))
    table.insert(health.messages, "  - Hover: " .. tostring(cfg.hover.enable))

    -- Check for open SuperTOML buffers
    local super_toml_bufs = 0
    for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_is_loaded(bufnr) and lsp.is_super_toml_buffer(bufnr) then
            super_toml_bufs = super_toml_bufs + 1
        end
    end
    table.insert(health.messages, string.format("✓ Open SuperTOML buffers: %d", super_toml_bufs))

    -- Print health report
    vim.notify(
        "[supertoml-analyzer] Health Check\n" .. table.concat(health.messages, "\n"),
        health.ok and vim.log.levels.INFO or vim.log.levels.ERROR
    )

    return health
end

--- Get information about the plugin
---@return table
function M.info()
    return {
        version = M._VERSION,
        server_name = lsp.server_name,
        server_status = lsp.status(),
        client_id = lsp.client_id,
        config = config.get(),
        filetype = lsp.get_filetype(),
        file_patterns = lsp.file_patterns,
    }
end

return M
