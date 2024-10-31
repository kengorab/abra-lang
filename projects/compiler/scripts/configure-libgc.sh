#!/usr/bin/env bash

# Clone and configure [libgc](https://github.com/ivmai/bdwgc)

set -e # exit on non-zero exit status

SCRIPT_DIR=$(dirname "$(readlink -f "$0")")

# cd to compiler root
cd "$SCRIPT_DIR/.."

EXT_DIR="ext"

echo "Make $EXT_DIR dir"
echo "============"
if [ -d "$EXT_DIR" ]; then
  echo "Removing existing $EXT_DIR directory"
  rm -rf "$EXT_DIR"
fi
mkdir "$EXT_DIR"
cd "$EXT_DIR"

echo "Cloning bdwgc repo"
echo "=================="
git clone --depth=1 https://github.com/ivmai/bdwgc.git
rm -rf bdwgc/.git
cd bdwgc
git clone --depth=1 https://github.com/ivmai/libatomic_ops.git
rm -rf libatomic_ops/.git
echo "Configuring bdwgc"
ls -la
./autogen.sh
./configure
make -j
make check

echo "Generate gc.a"
make -f Makefile.direct base_lib

echo "All done!"
