
if [[ $1 == "js"  ]]; then
    if [[ "$OSTYPE" != "darwin"* && $in_nix != 0 ]]; then
    	sed -i '' "s/import require\$\$1\$3 from '\.\.\/package\.json';/import require\$\$1\$3 from '..\/package.json' with {type: \"json\"};/" clients/javascript/open-feature-provider/dist/index.esm.js
    else
    	sed -i "s/import require\$\$1\$3 from '\.\.\/package\.json';/import require\$\$1\$3 from '..\/package.json' with {type: \"json\"};/" clients/javascript/open-feature-provider/dist/index.esm.js
    fi
    mkdir -p clients/javascript/open-feature-provider/dist/native-lib
fi

COPY_PATH=""
if [[ $1 == "js" ]]; then
    COPY_PATH="clients/javascript/open-feature-provider/dist/native-lib"
elif [[ $1 == "py" ]]; then
    COPY_PATH="clients/python/provider-sdk-tests/.venv/lib/python3.12/site-packages/superposition_bindings"
fi

if [[ "$OSTYPE" != "darwin"* ]]; then
    cp ./target/debug/libsuperposition_core.so $COPY_PATH/libsuperposition_core-x86_64-unknown-linux-gnu.so
else
    cp ./target/debug/libsuperposition_core.dylib $COPY_PATH/libsuperposition_core-aarch64-apple-darwin.dylib
fi
