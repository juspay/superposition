#!/bin/bash
# Script to enforce PascalCase naming convention for Leptos component functions
# This ensures consistency between component definitions and their usage in JSX

set -e

FRONTEND_SRC="crates/frontend/src"
ERRORS=0

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "Checking Leptos component naming conventions..."

# Find all component functions with snake_case names
while IFS= read -r file; do
    if [ -f "$file" ]; then
        # Look for #[component] followed by pub fn snake_case_name
        while IFS= read -r line_num; do
            # Get the function name from the line
            func_line=$(sed -n "${line_num}p" "$file")
            func_name=$(echo "$func_line" | sed -n 's/.*pub fn \([a-z_][a-z0-9_]*\).*/\1/p')

            # Check if it starts with lowercase (snake_case)
            if [[ "$func_name" =~ ^[a-z] ]]; then
                # Convert to PascalCase for the suggestion
                pascal_name=$(echo "$func_name" | sed -r 's/(^|_)([a-z])/\U\2/g')

                rel_file="${file#./}"
                echo -e "${RED}✗${NC} Found snake_case component in ${YELLOW}${rel_file}:${line_num}${NC}"
                echo -e "  Function: ${RED}${func_name}${NC}"
                echo -e "  Expected: ${GREEN}${pascal_name}${NC}"
                echo ""
                ERRORS=$((ERRORS + 1))
            fi
        done < <(grep -n "#\[component\]" "$file" | while IFS=: read -r line_num _; do
            # Look at next few lines for the function definition
            for offset in 1 2 3 4; do
                next_line=$((line_num + offset))
                if sed -n "${next_line}p" "$file" | grep -q "pub fn [a-z_][a-z0-9_]*"; then
                    echo "$next_line"
                    break
                fi
            done
        done)
    fi
done < <(find "$FRONTEND_SRC" -name "*.rs" -type f)

if [ $ERRORS -eq 0 ]; then
    echo -e "${GREEN}✓${NC} All Leptos components use PascalCase naming convention!"
    exit 0
else
    echo -e "${RED}✗${NC} Found $ERRORS component(s) with incorrect naming."
    echo ""
    echo "Leptos components should use PascalCase for consistency with their usage in JSX."
    echo "Please rename these functions to match the PascalCase convention."
    echo ""
    echo "Example:"
    echo "  ${RED}pub fn my_component${NC} → ${GREEN}pub fn MyComponent${NC}"
    exit 1
fi
