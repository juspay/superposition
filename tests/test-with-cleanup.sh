#!/bin/bash

# Parse flags
ORG_PATTERN="org%"
WORKSPACE_NAME=""
CLEANUP_ONLY=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --cleanup-only)
            CLEANUP_ONLY=true
            shift
            ;;
        --org-pattern)
            if [[ -z "$2" || "$2" == --* ]]; then
                echo "Error: --org-pattern requires a value"
                exit 1
            fi
            ORG_PATTERN="$2"
            shift 2
            ;;
        --workspace)
            if [[ -z "$2" || "$2" == --* ]]; then
                echo "Error: --workspace requires a value"
                exit 1
            fi
            WORKSPACE_NAME="$2"
            shift 2
            ;;
        *)
            echo "Unknown argument: $1"
            echo "Usage: $0 [--cleanup-only] [--org-pattern pattern] [--workspace name]"
            exit 1
            ;;
    esac
done

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

# Function to run cleanup
run_cleanup() {
    echo ""
    echo "========================================"
    echo "Running cleanup..."
    if [ -n "$WORKSPACE_NAME" ]; then
        echo "Mode: Specific Workspace ($WORKSPACE_NAME) in Orgs ($ORG_PATTERN)"
    else
        echo "Mode: Full Organization Cleanup ($ORG_PATTERN)"
    fi
    echo "========================================"
    
    psql "$DATABASE_URL" \
        -v org_pattern="$ORG_PATTERN" \
        -v workspace_name="$WORKSPACE_NAME" \
        -f cleanup.sql
        
    CLEANUP_EXIT_CODE=$?
    
    if [ $CLEANUP_EXIT_CODE -eq 0 ]; then
        echo ""
        echo "✓ Cleanup completed"
    else
        echo ""
        echo "✗ Cleanup failed (exit code: $CLEANUP_EXIT_CODE)"
    fi
    return $CLEANUP_EXIT_CODE
}

if [ "$CLEANUP_ONLY" = true ]; then
    run_cleanup
    exit $?
fi

echo "Running tests..."
bun test
TEST_EXIT_CODE=$?

if [ $TEST_EXIT_CODE -eq 0 ]; then
    run_cleanup
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
