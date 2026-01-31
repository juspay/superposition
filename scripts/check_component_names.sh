#!/bin/bash
# Script to enforce PascalCase naming convention for Leptos component functions
# This ensures consistency between component definitions and their usage in JSX
#
# Usage:
#   ./check_component_names.sh        # Check only
#   ./check_component_names.sh --fix  # Auto-convert to PascalCase

set -e

FRONTEND_SRC="crates/frontend/src"
ERRORS=0
FIX_MODE=false

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Parse command line arguments
if [[ "$1" == "--fix" ]] || [[ "$1" == "-f" ]]; then
    FIX_MODE=true
    echo "Running in FIX mode - will auto-convert snake_case to PascalCase"
    echo ""
fi

echo "Checking Leptos component naming conventions..."
echo ""

# Convert snake_case to PascalCase
snake_to_pascal() {
    echo "$1" | perl -pe 's/(^|_)([a-z])/uc($2)/ge'
}

# Find all Rust files and check for component patterns
while IFS= read -r file; do
    if [ -f "$file" ]; then
        # Read file content and search for #[component] followed by fn
        content=$(cat "$file")
        
        # Use perl for multiline regex matching with strict pattern: #\[component\]\n.*fn (.*)\(
        while IFS='|' read -r line_num func_name; do
            if [ -z "$func_name" ]; then
                continue
            fi
            
            # Validate against PascalCase pattern: ^[A-Z][A-Za-z0-9]*$
            if ! [[ "$func_name" =~ ^[A-Z][A-Za-z0-9]*$ ]]; then
                # Convert to PascalCase for the suggestion
                pascal_name=$(snake_to_pascal "$func_name")
                
                rel_file="${file#./}"
                echo -e "${RED}✗${NC} Invalid component name in ${YELLOW}${rel_file}:${line_num}${NC}"
                echo -e "  Function: ${RED}${func_name}${NC}"
                echo -e "  Expected: ${GREEN}${pascal_name}${NC}"
                
                if [ "$FIX_MODE" = true ]; then
                    echo -e "  ${GREEN}Fixing...${NC}"
                    # Use perl for in-place replacement to handle the specific function name
                    perl -i -pe "s/fn ${func_name}\(/fn ${pascal_name}(/g" "$file"
                    echo -e "  ${GREEN}✓ Converted${NC}"
                fi
                
                echo ""
                ERRORS=$((ERRORS + 1))
            fi
        done < <(echo "$content" | perl -0777 -ne '
            # Use the strict regex pattern: #\[component\]\n.*fn (.*)\(
            # The .* does not match newlines (no /s flag in the part after \n)
            while (/#\[component\]\n[^\n]*fn\s+(\w+)\s*\(/g) {
                my $func_name = $1;
                my $pos = pos($_);
                my $line_num = 1 + (() = substr($_, 0, $pos) =~ /\n/g);
                print "$line_num|$func_name\n";
            }
        ')
    fi
done < <(find "$FRONTEND_SRC" -name "*.rs" -type f)

if [ $ERRORS -eq 0 ]; then
    echo -e "${GREEN}✓${NC} All Leptos components use PascalCase naming convention!"
    exit 0
else
    if [ "$FIX_MODE" = true ]; then
        echo -e "${YELLOW}⚠${NC}  Fixed $ERRORS component(s) - please review changes and update component usages."
        exit 0
    else
        echo -e "${RED}✗${NC} Found $ERRORS component(s) with incorrect naming."
        echo ""
        echo "Leptos components should use PascalCase for consistency with their usage in JSX."
        echo "Please rename these functions to match the PascalCase convention."
        echo ""
        echo "You can also run with --fix to automatically convert:"
        echo -e "  ${YELLOW}./scripts/check_component_names.sh --fix${NC}"
        echo ""
        echo "Example:"
        echo -e "  ${RED}pub fn my_component${NC} → ${GREEN}pub fn MyComponent${NC}"
        exit 1
    fi
fi
