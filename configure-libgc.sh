# Clone and configure [libgc](https://github.com/ivmai/bdwgc)
# The end result of this script is to build the gc.a lib file
# and copy that file along with the include/ directory into
# abra_core/src/transpile/target/c.

EXT_DIR="ext"
C_TARGET_ROOT="abra_core/src/transpile/target/c"
LLVM_TARGET_ROOT="abra_llvm/ext"

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
git clone https://github.com/ivmai/bdwgc.git
cd bdwgc
git clone https://github.com/ivmai/libatomic_ops.git
echo "Configuring bdwgc"
./autogen.sh
./configure
make -j
make check

echo "Generate gc.a"
make -f Makefile.direct base_lib

cd ../..

echo "Copying files into place"
echo "========================"
if [ ! -d "$C_TARGET_ROOT/libgc" ]; then
  echo "Creating $C_TARGET_ROOT/libgc directory"
  mkdir "$C_TARGET_ROOT/libgc"
fi
if [ ! -d "$LLVM_TARGET_ROOT" ]; then
  mkdir "$LLVM_TARGET_ROOT"
fi
if [ ! -d "$LLVM_TARGET_ROOT/libgc" ]; then
  echo "Creating $LLVM_TARGET_ROOT/libgc directory"
  mkdir "$LLVM_TARGET_ROOT/libgc"
fi

if [ -d "$C_TARGET_ROOT/libgc/include" ]; then
  echo "Removing existing $C_TARGET_ROOT/libgc/include directory"
  rm -rf "$C_TARGET_ROOT/libgc/include"
fi
cp -R "$EXT_DIR/bdwgc/include" "$C_TARGET_ROOT/libgc/."
if [ -d "$LLVM_TARGET_ROOT/libgc/include" ]; then
  echo "Removing existing $LLVM_TARGET_ROOT/libgc/include directory"
  rm -rf "$LLVM_TARGET_ROOT/libgc/include"
fi
cp -R "$EXT_DIR/bdwgc/include" "$LLVM_TARGET_ROOT/libgc/."

if [ -d "$C_TARGET_ROOT/libgc/lib" ]; then
  echo "Removing existing $C_TARGET_ROOT/libgc/lib directory"
  rm -rf "$C_TARGET_ROOT/libgc/lib"
fi
mkdir "$C_TARGET_ROOT/libgc/lib"
cp "$EXT_DIR/bdwgc/libgc.a" "$C_TARGET_ROOT/libgc/lib/."
if [ -d "$LLVM_TARGET_ROOT/libgc/lib" ]; then
  echo "Removing existing $LLVM_TARGET_ROOT/libgc/lib directory"
  rm -rf "$LLVM_TARGET_ROOT/libgc/lib"
fi
mkdir "$LLVM_TARGET_ROOT/libgc/lib"
cp "$EXT_DIR/bdwgc/libgc.a" "$LLVM_TARGET_ROOT/libgc/lib/."

echo "All done!"
