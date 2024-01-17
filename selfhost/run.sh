#!/usr/bin/env sh

cd ..
cargo b
cd selfhost
../target/debug/abra build -r ./src/main.abra -- "$1"
