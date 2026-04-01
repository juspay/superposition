# Superposition Skills

<p align="center">
<img src="https://juspay.io/images/superposition/logo.jpg" alt="Superposition Logo" width="300">
</p>

**AI agent skills for [Superposition](https://github.com/juspay/superposition)** — the context-aware configuration and experimentation platform by Juspay.

Give your AI coding agent deep expertise in Superposition so it can help you manage configurations, run experiments, integrate SDKs, and deploy — all with best practices baked in.

---

## Installation

### Using the Skills CLI (Recommended)

Works with Claude Code, Cursor, Gemini CLI, Codex CLI, Windsurf, OpenCode, and more. Auto-detects your AI harness.

```bash
npx skills add juspay/superposition
```

**Options:**
```bash
# Install globally (available in all projects)
npx skills add juspay/superposition -g

# Install to specific agents
npx skills add juspay/superposition --agent claude-code cursor

# Install specific skills only
npx skills add juspay/superposition --skill superposition-config superposition-experiments

# Install everything (all skills, all agents)
npx skills add juspay/superposition --all
```

### Claude Code Plugin

```bash
# Add marketplace
/plugin marketplace add juspay/superposition

# Then open /plugin to install from Discover tab
```

### Manual Install

<details>
<summary>Clone and symlink</summary>

```bash
# Clone the Superposition repo
git clone https://github.com/juspay/superposition.git ~/superposition

# Project-level (recommended)
mkdir -p .claude/skills
ln -s ~/superposition/skills/superposition-* .claude/skills/

# OR global
mkdir -p ~/.claude/skills
ln -s ~/superposition/skills/superposition-* ~/.claude/skills/
```
</details>

<details>
<summary>Tool-specific paths</summary>

| Tool | Project Path | Global Path |
|------|--------------|-------------|
| Claude Code | `.claude/skills/` | `~/.claude/skills/` |
| Cursor | `.cursor/skills/` | `~/.cursor/skills/` |
| GitHub Copilot | `.github/skills/` | `~/.copilot/skills/` |
| Gemini CLI | `.gemini/skills/` | `~/.gemini/skills/` |
| Codex CLI | `.codex/skills/` | `~/.codex/skills/` |
| Windsurf | `.windsurf/skills/` | `~/.codeium/windsurf/skills/` |
| OpenCode | `.opencode/skills/` | `~/.config/opencode/skills/` |
</details>

---

## Compatibility

These skills follow the universal [SKILL.md spec](https://agentskills.io/specification) and work with any AI coding assistant that supports agent skills.

---

## Available Skills

| Skill | Description | Use When |
|-------|-------------|----------|
| [superposition-config](superposition-config/) | Context-aware configuration management | Creating dimensions, default configs, contexts, overrides, functions, type templates |
| [superposition-experiments](superposition-experiments/) | Experimentation and A/B testing | Creating experiments, managing variants, controlling traffic, webhooks |
| [superposition-sdk](superposition-sdk/) | SDK usage across languages | Using the Superposition SDK in Rust, JavaScript, Python, or Java |
| [superposition-provider](superposition-provider/) | OpenFeature provider integration | Consuming configurations in your application via OpenFeature |
| [superposition-setup](superposition-setup/) | Setup, deployment, and operations | Installing, configuring, deploying, and operating Superposition |
| [superposition-api](superposition-api/) | REST API reference | Making direct API calls to the Superposition platform |

---

## Example Prompts

Once skills are installed, try asking your AI agent:

- *"Create a context-aware configuration for a ride-hailing app with city-based pricing. Use superposition-config skill."*
- *"Set up an A/B test for our checkout flow with 3 variants. Use superposition-experiments skill."*
- *"Integrate Superposition in our Python FastAPI service using OpenFeature. Use superposition-provider skill."*
- *"Create a dimension for user tier (bronze/silver/gold) with validation. Use superposition-config skill."*
- *"Deploy Superposition with Docker Compose for our staging environment. Use superposition-setup skill."*
- *"List all active experiments via the API. Use superposition-api skill."*

---

## Updating

```bash
# Using skills CLI
npx skills update

# Or if cloned manually
cd ~/superposition && git pull
```

---

## License

Apache-2.0 — same as [Superposition](https://github.com/juspay/superposition).

## Contact

- [superposition@juspay.in](mailto:superposition@juspay.in)
- [Discord](https://discord.gg/jNeUJR9Bwr)
- [GitHub Issues](https://github.com/juspay/superposition/issues)
