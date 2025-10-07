if [[ "$OSTYPE" != "darwin"* && $in_nix != 0 ]]; then
	sed -i '' "s/import require\$\$1\$3 from '\.\.\/package\.json';/import require\$\$1\$3 from '..\/package.json' with {type: \"json\"};/" clients/javascript/open-feature-provider/dist/index.esm.js
else
	sed -i "s/import require\$\$1\$3 from '\.\.\/package\.json';/import require\$\$1\$3 from '..\/package.json' with {type: \"json\"};/" clients/javascript/open-feature-provider/dist/index.esm.js
 fi
 
mkdir -p clients/javascript/open-feature-provider/dist/native-lib
if [[ "$OSTYPE" != "darwin"* ]]; then
    mv ./target/debug/libsuperposition_core.so clients/javascript/open-feature-provider/dist/native-lib/libsuperposition_core-x86_64-unknown-linux-gnu.so
else
    mv ./target/debug/libsuperposition_core.dylib clients/javascript/open-feature-provider/dist/native-lib/libsuperposition_core-aarch64-apple-darwin.dylib
fi
