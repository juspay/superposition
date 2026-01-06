#!/bin/bash

set -e

echo "Running tests..."
bun test
TEST_EXIT_CODE=$?

if [ $TEST_EXIT_CODE -eq 0 ]; then
    echo ""
    echo "========================================"
    echo "Tests passed. Running cleanup..."
    echo "========================================"
    bun run-cleanup.ts
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