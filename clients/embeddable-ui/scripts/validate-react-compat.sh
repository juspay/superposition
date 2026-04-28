#!/usr/bin/env bash

set -euo pipefail

react_version="${REACT_VERSION:-18.3.1}"
react_dom_version="${REACT_DOM_VERSION:-$react_version}"
react_types_version="${REACT_TYPES_VERSION:-18.3.12}"
react_dom_types_version="${REACT_DOM_TYPES_VERSION:-18.3.1}"
cache_dir="${TMPDIR:-/tmp}/superposition-embeddable-ui-npm-cache"

mkdir -p "$cache_dir"

echo "Validating with react@$react_version, react-dom@$react_dom_version, @types/react@$react_types_version, @types/react-dom@$react_dom_types_version"

npm install \
  --no-save \
  --no-package-lock \
  --cache "$cache_dir" \
  "react@${react_version}" \
  "react-dom@${react_dom_version}" \
  "@types/react@${react_types_version}" \
  "@types/react-dom@${react_dom_types_version}"

npm run typecheck
npm run test
npm run build
npm pack --dry-run --cache "$cache_dir"
