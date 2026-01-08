#!/bin/bash

# Load environment variables from root .env file
if [ -f ../.env ]; then
    set -a
    . ../.env
    set +a
fi

# Build DATABASE_URL from individual env vars if not set
if [ -z "$DATABASE_URL" ]; then
    DB_HOST="${DB_HOST:-localhost:5432}"
    DB_NAME="${DB_NAME:-config}"
    DB_USER="${DB_USER:-postgres}"
    DB_PASSWORD="${DB_PASSWORD:-docker}"
    DATABASE_URL="postgres://${DB_USER}:${DB_PASSWORD}@${DB_HOST}/${DB_NAME}?sslmode=disable"
fi

echo "Running tests..."
bun test
TEST_EXIT_CODE=$?

if [ $TEST_EXIT_CODE -eq 0 ]; then
    echo ""
    echo "========================================"
    echo "Tests passed. Running cleanup..."
    echo "========================================"
    psql "$DATABASE_URL" -f cleanup.sql
    CLEANUP_EXIT_CODE=$?
    
    if [ $CLEANUP_EXIT_CODE -eq 0 ]; then
        echo ""
        echo "✓ Tests passed and cleanup completed"
    else
        echo ""
        echo "⚠ Tests passed but cleanup failed (exit code: $CLEANUP_EXIT_CODE)"
    fi
else
    echo ""
    echo "========================================"
    echo "Tests failed (exit code: $TEST_EXIT_CODE)"
    echo "Skipping cleanup to preserve data for debugging"
    echo "========================================"
fi

exit $TEST_EXIT_CODE