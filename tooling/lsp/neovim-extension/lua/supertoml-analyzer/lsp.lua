local config = require("supertoml-analyzer.config")

local M = {}

--- Server name for the LSP client
M.server_name = "supertoml_analyzer"

--- Active client ID
M.client_id = nil

--- Autocmd group for SuperTOML
M.augroup = nil

--- File patterns for SuperTOML
M.file_patterns = {
    "*.super.toml",
    "*.stoml",
    "super.toml",
}

--- Check if a buffer is a SuperTOML file
---@param bufnr integer
---@return boolean
function M.is_super_toml_buffer(bufnr)
    bufnr = bufnr or 0
    local filename = vim.api.nvim_buf_get_name(bufnr)
    local basename = vim.fs.basename(filename)

    -- Check extensions
    if filename:match("%.super%.toml$") or filename:match("%.stoml$") then
        return true
    end

    -- Check exact filename
    if basename == "super.toml" then
        return true
    end

    return false
end

--- Get the file type for SuperTOML files
---@return string
function M.get_filetype()
    return "superTOML"
end

--- Start the LSP server
---@param opts table|nil Optional overrides
---@return integer|nil client_id
function M.start(opts)
    opts = opts or {}

    -- If already running, return existing client
    if M.client_id then
        local client = vim.lsp.get_client_by_id(M.client_id)
        if client and client:is_stopped() == false then
            return M.client_id
        end
    end

    local cmd = config.get_server_cmd()

    -- Verify the command is executable
    local binary = cmd[1]
    if vim.fn.executable(binary) ~= 1 then
        vim.notify(
            "[supertoml-analyzer] Cannot find 'supertoml-analyzer' binary.\n"
                .. "Please build it with 'cargo build -p supertoml_lsp' or set the path in config.",
            vim.log.levels.ERROR
        )
        return nil
    end

    -- Get server config
    local cfg = config.get()

    -- Create the client config
    local client_config = {
        name = M.server_name,
        cmd = cmd,
        cmd_env = cfg.server.env,
        root_dir = M.find_root_dir(),
        initialization_options = config.get_init_options(),
        capabilities = config.get_capabilities(),
        settings = {},
        flags = {
            debounce_text_changes = 150,
        },
        on_init = function(client, result)
            M.on_init(client, result)
        end,
        on_exit = function(code, signal, client_id)
            M.on_exit(code, signal, client_id)
        end,
        on_attach = function(client, bufnr)
            M.on_attach(client, bufnr, cfg.on_attach)
        end,
    }

    -- Start the client
    M.client_id = vim.lsp.start_client(client_config)

    if not M.client_id then
        vim.notify(
            "[supertoml-analyzer] Failed to start language server.",
            vim.log.levels.ERROR
        )
        return nil
    end

    return M.client_id
end

--- Stop the LSP server
---@return boolean
function M.stop()
    if not M.client_id then
        return false
    end

    local client = vim.lsp.get_client_by_id(M.client_id)
    if client then
        client:stop()
    end

    M.client_id = nil
    return true
end

--- Restart the LSP server
---@return integer|nil client_id
function M.restart()
    M.stop()

    -- Small delay to ensure clean shutdown
    vim.defer_fn(function()
        M.start()
        M.attach_to_buffers()
    end, 100)

    return M.client_id
end

--- Attach the LSP to a specific buffer
---@param bufnr integer
---@return boolean
function M.attach(bufnr)
    bufnr = bufnr or 0

    -- Ensure server is running
    if not M.client_id then
        M.start()
    end

    if not M.client_id then
        return false
    end

    -- Check if already attached
    local clients = vim.lsp.get_clients({ bufnr = bufnr, name = M.server_name })
    if #clients > 0 then
        return true
    end

    -- Attach to buffer
    local ok = vim.lsp.buf_attach_client(bufnr, M.client_id)
    if not ok then
        vim.notify(
            "[supertoml-analyzer] Failed to attach to buffer " .. bufnr,
            vim.log.levels.WARN
        )
    end

    return ok
end

--- Attach to all open SuperTOML buffers
---@return integer count Number of buffers attached
function M.attach_to_buffers()
    local count = 0

    for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_is_loaded(bufnr) and M.is_super_toml_buffer(bufnr) then
            if M.attach(bufnr) then
                count = count + 1
            end
        end
    end

    return count
end

--- Find the root directory for the LSP
---@return string
function M.find_root_dir()
    local bufname = vim.api.nvim_buf_get_name(0)
    local bufdir = vim.fs.dirname(bufname)

    -- Look for markers from the current file's directory upward
    local markers = { ".git", "Cargo.toml", "super.toml" }

    for _, marker in ipairs(markers) do
        local found = vim.fs.find(marker, {
            path = bufdir,
            upward = true,
            stop = vim.env.HOME,
        })

        if #found > 0 then
            return vim.fs.dirname(found[1])
        end
    end

    -- Fallback to current working directory
    return vim.fn.getcwd()
end

--- Called when the LSP client initializes
---@param client table
---@param result table
function M.on_init(client, result)
    vim.notify(
        "[supertoml-analyzer] Language server initialized successfully.",
        vim.log.levels.INFO
    )
end

--- Called when the LSP client exits
---@param code integer
---@param signal integer
---@param client_id integer
function M.on_exit(code, signal, client_id)
    if code ~= 0 then
        vim.notify(
            string.format("[supertoml-analyzer] Language server exited with code %d", code),
            vim.log.levels.WARN
        )
    end

    if client_id == M.client_id then
        M.client_id = nil
    end
end

--- Called when the LSP attaches to a buffer
---@param client table
---@param bufnr integer
---@param user_callback function|nil
function M.on_attach(client, bufnr, user_callback)
    -- Set up buffer-local keymaps (if desired)
    M.setup_buffer_keymaps(bufnr)

    -- Set up buffer-local autocommands
    M.setup_buffer_autocmds(bufnr)

    -- Call user callback if provided
    if user_callback then
        user_callback(client, bufnr)
    end
end

--- Set up buffer-local keymaps
---@param bufnr integer
function M.setup_buffer_keymaps(bufnr)
    local opts = { buffer = bufnr, noremap = true, silent = true }

    -- Hover
    vim.keymap.set("n", "K", vim.lsp.buf.hover, vim.tbl_extend("keep", { desc = "Show hover" }, opts))

    -- Go to definition
    vim.keymap.set("n", "gd", vim.lsp.buf.definition, vim.tbl_extend("keep", { desc = "Go to definition" }, opts))

    -- Go to references
    vim.keymap.set("n", "gr", vim.lsp.buf.references, vim.tbl_extend("keep", { desc = "Find references" }, opts))

    -- Rename
    vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, vim.tbl_extend("keep", { desc = "Rename symbol" }, opts))

    -- Code actions
    vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, vim.tbl_extend("keep", { desc = "Code actions" }, opts))

    -- Format
    vim.keymap.set("n", "<leader>f", function()
        vim.lsp.buf.format({ async = true })
    end, vim.tbl_extend("keep", { desc = "Format buffer" }, opts))

    -- Diagnostics navigation
    vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, vim.tbl_extend("keep", { desc = "Previous diagnostic" }, opts))
    vim.keymap.set("n", "]d", vim.diagnostic.goto_next, vim.tbl_extend("keep", { desc = "Next diagnostic" }, opts))

    -- Show diagnostics
    vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, vim.tbl_extend("keep", { desc = "Show diagnostics" }, opts))
end

--- Set up buffer-local autocommands
---@param bufnr integer
function M.setup_buffer_autocmds(bufnr)
    local augroup = vim.api.nvim_create_augroup("SuperTOMLBuffer_" .. bufnr, { clear = true })

    -- Format on save (optional - can be configured)
    vim.api.nvim_create_autocmd("BufWritePre", {
        group = augroup,
        buffer = bufnr,
        desc = "Format SuperTOML on save",
        callback = function()
            -- Only format if diagnostics are enabled
            local cfg = config.get()
            if cfg.diagnostics.enable then
                vim.lsp.buf.format({ bufnr = bufnr, timeout_ms = 1000 })
            end
        end,
    })
end

--- Set up autocmds for auto-starting the LSP
function M.setup_autocmds()
    if M.augroup then
        vim.api.nvim_del_augroup_by_id(M.augroup)
    end

    M.augroup = vim.api.nvim_create_augroup("SuperTOMLLSP", { clear = true })

    -- Auto-start LSP when opening SuperTOML files
    vim.api.nvim_create_autocmd("FileType", {
        group = M.augroup,
        pattern = M.get_filetype(),
        desc = "Start SuperTOML LSP for SuperTOML files",
        callback = function(args)
            M.attach(args.buf)
        end,
    })

    -- Also check BufReadPost for files that might not have the correct filetype yet
    vim.api.nvim_create_autocmd("BufReadPost", {
        group = M.augroup,
        pattern = M.file_patterns,
        desc = "Detect SuperTOML files",
        callback = function(args)
            if M.is_super_toml_buffer(args.buf) then
                vim.api.nvim_buf_set_option(args.buf, "filetype", M.get_filetype())
            end
        end,
    })

    -- Auto-attach to newly created SuperTOML files
    vim.api.nvim_create_autocmd("BufNewFile", {
        group = M.augroup,
        pattern = M.file_patterns,
        desc = "Set filetype for new SuperTOML files",
        callback = function(args)
            vim.api.nvim_buf_set_option(args.buf, "filetype", M.get_filetype())
        end,
    })
end

--- Check if the server is running
---@return boolean
function M.is_running()
    if not M.client_id then
        return false
    end

    local client = vim.lsp.get_client_by_id(M.client_id)
    return client ~= nil and not client:is_stopped()
end

--- Get server status
---@return string
function M.status()
    if M.is_running() then
        return "running"
    elseif M.client_id then
        return "stopped"
    else
        return "not started"
    end
end

--- Log a message to the LSP log
---@param message string
---@param level string|nil
function M.log(message, level)
    level = level or "info"
    local log_levels = {
        trace = vim.log.levels.TRACE,
        debug = vim.log.levels.DEBUG,
        info = vim.log.levels.INFO,
        warn = vim.log.levels.WARN,
        error = vim.log.levels.ERROR,
    }
    vim.notify("[supertoml-analyzer] " .. message, log_levels[level] or vim.log.levels.INFO)
end

return M
