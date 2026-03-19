#!/usr/bin/env bash
# SessionStart hook: bootstraps the dev environment for Claude Code sessions.
# This runs at the start of every session (local and cloud).
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

# Ensure .env exists
if [ ! -f .env ]; then
    cp .env.example .env
    echo "Created .env from .env.example"
fi

# Install Node dependencies if needed
if [ ! -d node_modules ]; then
    echo "Installing npm dependencies..."
    npm ci --silent
fi

# Print environment summary
echo "--- Superposition Dev Environment ---"
echo "Rust:    $(rustc --version 2>/dev/null || echo 'not installed')"
echo "Node:    $(node --version 2>/dev/null || echo 'not installed')"
echo "Branch:  $(git branch --show-current)"
echo "DB:      $(pg_isready -h localhost -p 5432 2>/dev/null && echo 'ready' || echo 'not running')"
echo "Redis:   $(redis-cli -p 6379 ping 2>/dev/null || echo 'not running')"
echo "---"
