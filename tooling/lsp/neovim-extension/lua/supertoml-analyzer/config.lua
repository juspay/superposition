superposition/tooling/lsp/neovim-extension/lua/supertoml-analyzer/config.lua
---@class SuperTOMLConfig
---@field server SuperTOMLServerConfig
---@field diagnostics SuperTOMLDiagnosticsConfig
---@field completions SuperTOMLCompletionsConfig
---@field hover SuperTOMLHoverConfig
---@field on_attach fun(client: table, bufnr: integer)|nil Callback when LSP attaches to a buffer
---@field capabilities table|nil Custom LSP capabilities
---@field log_level string Log level for the LSP client

---@class SuperTOMLServerConfig
---@field path string|nil Path to the supertoml-analyzer binary
---@field cmd table|nil Custom command to start the server (overrides path)
---@field env table|nil Environment variables for the server process
---@field timeout integer Timeout in milliseconds for server startup

---@class SuperTOMLDiagnosticsConfig
---@field enable boolean Enable diagnostics for SuperTOML files

---@class SuperTOMLCompletionsConfig
---@field enable boolean Enable auto-completions for SuperTOML files

---@class SuperTOMLHoverConfig
---@field enable boolean Enable hover documentation for SuperTOML files

local M = {}

--- Default configuration
M.defaults = {
    server = {
        path = nil,  -- Auto-detect if nil
        cmd = nil,   -- Custom command if provided
        env = nil,   -- Additional environment variables
        timeout = 10000,  -- 10 seconds
    },
    diagnostics = {
        enable = true,
    },
    completions = {
        enable = true,
    },
    hover = {
        enable = true,
    },
    on_attach = nil,  -- User callback for LSP attachment
    capabilities = nil,  -- Will be merged with default capabilities
    log_level = "info",  -- "trace", "debug", "info", "warn", "error"
}

--- Current configuration (merged with user config)
M.current = vim.deepcopy(M.defaults)

--- Valid log levels
M.valid_log_levels = {
    trace = vim.lsp.log_levels.TRACE,
    debug = vim.lsp.log_levels.DEBUG,
    info = vim.lsp.log_levels.INFO,
    warn = vim.lsp.log_levels.WARN,
    error = vim.lsp.log_levels.ERROR,
    off = vim.lsp.log_levels.OFF,
}

--- Setup configuration with user overrides
---@param opts SuperTOMLConfig|nil User configuration options
---@return SuperTOMLConfig
function M.setup(opts)
    opts = opts or {}

    -- Deep merge user options with defaults
    M.current = vim.tbl_deep_extend("force", M.defaults, opts)

    -- Validate log level
    if M.valid_log_levels[M.current.log_level] == nil then
        vim.notify(
            "[supertoml-analyzer] Invalid log level '" .. M.current.log_level .. "'. Using 'info'.",
            vim.log.levels.WARN
        )
        M.current.log_level = "info"
    end

    return M.current
end

--- Get the current configuration
---@return SuperTOMLConfig
function M.get()
    return M.current
end

--- Get the server command to start the LSP
---@return table cmd The command to execute
function M.get_server_cmd()
    -- If custom command is provided, use it
    if M.current.server.cmd then
        return M.current.server.cmd
    end

    -- If path is provided, use it
    if M.current.server.path then
        return { M.current.server.path }
    end

    -- Try to find the binary
    local binary_name = "supertoml-analyzer"

    -- Check if binary exists in PATH
    if vim.fn.executable(binary_name) == 1 then
        return { binary_name }
    end

    -- Try common locations relative to the current working directory
    local cwd = vim.fn.getcwd()
    local possible_paths = {
        vim.fs.joinpath(cwd, "target", "debug", binary_name),
        vim.fs.joinpath(cwd, "target", "release", binary_name),
        vim.fs.joinpath(cwd, "..", "target", "debug", binary_name),
        vim.fs.joinpath(cwd, "..", "target", "release", binary_name),
    }

    -- Also check relative to the superposition repository if we can find it
    local superposition_root = M.find_superposition_root()
    if superposition_root then
        table.insert(possible_paths, 1, vim.fs.joinpath(superposition_root, "target", "debug", binary_name))
        table.insert(possible_paths, 2, vim.fs.joinpath(superposition_root, "target", "release", binary_name))
    end

    for _, path in ipairs(possible_paths) do
        if vim.fn.filereadable(path) == 1 and vim.fn.executable(path) == 1 then
            return { path }
        end
    end

    -- Fall back to binary name (will fail with a clear error message)
    return { binary_name }
end

--- Try to find the superposition repository root
---@return string|nil
function M.find_superposition_root()
    -- Try to find by looking for Cargo.toml with supertoml_lsp
    local current = vim.fn.getcwd()

    -- Walk up the directory tree
    local dir = current
    for _ = 1, 10 do  -- Limit depth to avoid infinite loop
        local cargo_toml = vim.fs.joinpath(dir, "Cargo.toml")
        if vim.fn.filereadable(cargo_toml) == 1 then
            -- Check if this looks like the superposition repo
            local content = vim.fn.readfile(cargo_toml)
            for _, line in ipairs(content) do
                if line:match("supertoml_lsp") or line:match("superposition") then
                    return dir
                end
            end
        end

        local parent = vim.fs.dirname(dir)
        if parent == dir then
            break  -- Reached root
        end
        dir = parent
    end

    return nil
end

--- Get initialization options for the LSP server
---@return table
function M.get_init_options()
    return {
        diagnostics = {
            enable = M.current.diagnostics.enable,
        },
        completions = {
            enable = M.current.completions.enable,
        },
        hover = {
            enable = M.current.hover.enable,
        },
        checkOnSave = {
            enable = true,
        },
    }
end

--- Get LSP capabilities
---@return table
function M.get_capabilities()
    -- Start with default capabilities
    local capabilities = vim.lsp.protocol.make_client_capabilities()

    -- Apply nvim-cmp capabilities if available
    local has_cmp, cmp_lsp = pcall(require, "cmp_nvim_lsp")
    if has_cmp then
        capabilities = cmp_lsp.default_capabilities(capabilities)
    end

    -- Merge with user-provided capabilities
    if M.current.capabilities then
        capabilities = vim.tbl_deep_extend("force", capabilities, M.current.capabilities)
    end

    return capabilities
end

--- Get the log level for LSP
---@return integer
function M.get_log_level()
    return M.valid_log_levels[M.current.log_level] or vim.lsp.log_levels.INFO
end

return M
