rm -rf pkg
wasm-pack build --target $WASM_PACK_TARGET

cp -R ts/* pkg/.
