---
sidebar_position: 11
title: Config File Compatibility
description: Evaluating common Linux/Mac configuration files that can be represented as SuperTOML
---

# Config File Compatibility

SuperTOML's core strengths — **typed key-value configs**, **dimensional context overrides**, and **cascading resolution** — map naturally to many existing configuration formats in the Linux/Mac ecosystem. This page evaluates which common config files are good candidates for representation as SuperTOML.

## Excellent Fit

These config files already use a cascading or override model that maps directly to SuperTOML's `[default-configs]` + `[dimensions]` + `[[overrides]]` structure.

### SSH Config (`~/.ssh/config`)

SSH config already cascades — `Host *` sets defaults and specific `Host` blocks override them. This maps directly to SuperTOML with a `host` dimension.

**Traditional format:**
```
Host *
    ServerAliveInterval 60
    ServerAliveCountMax 3
    ForwardAgent no
    IdentityFile ~/.ssh/id_ed25519

Host github.com
    IdentityFile ~/.ssh/github_key
    User git

Host staging-server
    Port 2222
    User deploy
    ForwardAgent yes
```

**As SuperTOML:**
```toml
[dimensions]
host = { position = 1, schema = { type = "string" } }

[default-configs]
ServerAliveInterval = { value = 60, schema = { type = "integer", minimum = 0 } }
ServerAliveCountMax = { value = 3, schema = { type = "integer", minimum = 1 } }
ForwardAgent = { value = false, schema = { type = "boolean" } }
IdentityFile = { value = "~/.ssh/id_ed25519", schema = { type = "string" } }
Port = { value = 22, schema = { type = "integer", minimum = 1, maximum = 65535 } }
User = { value = "root", schema = { type = "string" } }

[[overrides]]
_context_ = { host = "github.com" }
IdentityFile = "~/.ssh/github_key"
User = "git"

[[overrides]]
_context_ = { host = "staging-server" }
Port = 2222
User = "deploy"
ForwardAgent = true
```

**Benefits:** Schema validation catches invalid port numbers, enforces type correctness, and provides IDE autocomplete via LSP.

### EditorConfig (`.editorconfig`)

EditorConfig uses glob-based cascading (root defaults with file-type overrides). This maps to dimensions like `file_extension` and `file_path`.

**Traditional format:**
```ini
root = true

[*]
indent_style = space
indent_size = 4
charset = utf-8

[*.md]
trim_trailing_whitespace = false

[Makefile]
indent_style = tab
```

**As SuperTOML:**
```toml
[dimensions]
file_extension = { position = 1, schema = { type = "string", enum = ["md", "mk", "yml", "js", "py"] } }
filename = { position = 2, schema = { type = "string" } }

[default-configs]
indent_style = { value = "space", schema = { type = "string", enum = ["space", "tab"] } }
indent_size = { value = 4, schema = { type = "integer", enum = [2, 4, 8] } }
charset = { value = "utf-8", schema = { type = "string", enum = ["utf-8", "latin1", "utf-16be"] } }
trim_trailing_whitespace = { value = true, schema = { type = "boolean" } }

[[overrides]]
_context_ = { file_extension = "md" }
trim_trailing_whitespace = false

[[overrides]]
_context_ = { filename = "Makefile" }
indent_style = "tab"
```

### SSHD Config (`/etc/ssh/sshd_config`)

The `Match User/Group/Host/Address` blocks are literal dimensional overrides on top of global defaults.

**As SuperTOML:**
```toml
[dimensions]
user = { position = 2, schema = { type = "string" } }
group = { position = 3, schema = { type = "string" } }
address = { position = 1, schema = { type = "string" } }

[default-configs]
PermitRootLogin = { value = false, schema = { type = "boolean" } }
PasswordAuthentication = { value = false, schema = { type = "boolean" } }
MaxAuthTries = { value = 6, schema = { type = "integer", minimum = 1 } }

[[overrides]]
_context_ = { group = "admin" }
PermitRootLogin = true

[[overrides]]
_context_ = { address = "192.168.1.0/24" }
PasswordAuthentication = true
```

### Git Config (`~/.gitconfig`)

Git config has sections with typed key-values and conditional includes (`includeIf`) that map to dimension overrides (e.g., by `directory` or `remote_url`).

**As SuperTOML:**
```toml
[dimensions]
directory = { position = 1, schema = { type = "string" } }

[default-configs]
user_name = { value = "Dev User", schema = { type = "string" } }
user_email = { value = "dev@personal.com", schema = { type = "string", format = "email" } }
core_autocrlf = { value = "input", schema = { type = "string", enum = ["true", "false", "input"] } }
pull_rebase = { value = true, schema = { type = "boolean" } }

[[overrides]]
_context_ = { directory = "~/work/" }
user_email = "dev@company.com"
```

### Logrotate (`/etc/logrotate.conf`)

Global defaults with per-file override blocks. Dimensions: `log_file`. Values like `rotate`, `size`, and `compress` are all typed.

**As SuperTOML:**
```toml
[dimensions]
log_file = { position = 1, schema = { type = "string" } }

[default-configs]
rotate = { value = 4, schema = { type = "integer", minimum = 0 } }
frequency = { value = "weekly", schema = { type = "string", enum = ["daily", "weekly", "monthly"] } }
compress = { value = true, schema = { type = "boolean" } }
missingok = { value = false, schema = { type = "boolean" } }

[[overrides]]
_context_ = { log_file = "/var/log/nginx/*.log" }
rotate = 14
frequency = "daily"

[[overrides]]
_context_ = { log_file = "/var/log/syslog" }
rotate = 7
missingok = true
```

### Nginx Config (`nginx.conf`)

The `http` → `server` → `location` hierarchy is a cascading model. Dimensions: `server_name`, `location_path`.

**As SuperTOML:**
```toml
[dimensions]
server_name = { position = 2, schema = { type = "string" } }
location_path = { position = 1, schema = { type = "string" } }

[default-configs]
root = { value = "/var/www/html", schema = { type = "string" } }
worker_connections = { value = 1024, schema = { type = "integer", minimum = 1 } }
keepalive_timeout = { value = 65, schema = { type = "integer", minimum = 0 } }
gzip = { value = true, schema = { type = "boolean" } }

[[overrides]]
_context_ = { server_name = "api.example.com" }
root = "/var/www/api"

[[overrides]]
_context_ = { server_name = "api.example.com", location_path = "/v2" }
keepalive_timeout = 120
```

### Apache Config (`httpd.conf`)

`<VirtualHost>`, `<Directory>`, and `<Location>` blocks are context overrides following the same cascading model.

## Good Fit

These config files are key-value based and benefit from schema validation, even if their override model is simpler.

| Config File | Description | Dimensions |
|---|---|---|
| **`~/.npmrc` / `pip.conf`** | Package manager settings (`registry`, `strict-ssl`). Benefit is primarily schema validation. | `project`, `environment` |
| **systemd unit files** (`.service`, etc.) | INI-format key-values (`ExecStart`, `Restart`, `User`). Drop-in override directories already mimic cascading. | `environment`, `hostname` |
| **`/etc/sysctl.conf`** | Kernel parameters (`net.ipv4.ip_forward = 1`). Flat key-value. | `environment`, `host_role` |
| **`~/.tmux.conf`** | Terminal multiplexer options. Some commands are imperative, but option settings map well. | `terminal_type`, `os` |
| **`/etc/security/limits.conf`** | Resource limits with domain/type/item/value structure — semantically dimensional. | `user`, `group`, `limit_type` |
| **Docker Compose** | Service configs with typed values. The existing override file (`docker-compose.override.yml`) is essentially a manual cascade. | `environment`, `profile` |
| **`~/.Xresources`** | X11 resource settings. Wildcard/class matching is a crude cascade. | `application`, `display` |
| **`/etc/default/*`** (e.g., `grub`, `locale`) | Simple `KEY=VALUE` environment configs. | `machine_role` |

## Partial Fit

These can be represented but lose some expressiveness in the translation.

| Config File | Limitation |
|---|---|
| **`/etc/fstab`** | Tabular format (device, mountpoint, fstype, options). Could model as array of objects, but not a natural "config with overrides" use case. |
| **`/etc/sudoers`** | Rule-based with complex syntax (`user host=(runas) command`). The permission model doesn't map cleanly to key-value + dimensions. |
| **`/etc/crontab`** | Schedule expressions are job definitions, not key-value configurations. |
| **`/etc/rsyslog.conf`** | Rule-based routing (`facility.priority → action`). More of a pipeline definition than configuration. |

## Poor Fit

These files are fundamentally imperative or scripting-based and cannot be meaningfully represented as declarative configuration.

| Config File | Reason |
|---|---|
| **`~/.bashrc` / `~/.zshrc`** | Shell scripts with conditionals, functions, and aliases — not declarative config. |
| **`~/.vimrc` / `init.lua`** | Scripting languages (VimL, Lua), not key-value configuration. |
| **`Makefile`** | Build recipes with targets and dependencies, not configuration. |
| **`/etc/iptables/rules.v4`** | Ordered rule chains where order matters — fundamentally imperative. |

## What SuperTOML Adds

Across all compatible config files, SuperTOML provides consistent benefits:

- **Schema validation** — Catches typos like `Port = "yes"` or `indent_size = -1` at parse time
- **Deterministic priority resolution** — When multiple overrides match, the winner is unambiguous (unlike ad-hoc precedence rules in many formats)
- **IDE support via LSP** — Autocomplete, hover docs, and real-time validation for any config file represented as SuperTOML
- **Unified format** — One syntax and toolchain for all configuration, regardless of the target system
