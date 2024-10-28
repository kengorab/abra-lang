#!/usr/bin/env bash

set -e # exit on non-zero exit status

platform="$1"
if [ -z "$platform" ]; then
  echo "Missing required 'platform' argument (one of linux, darwin-x86, darwin-arm64)"
  exit 1
fi

if [[ "$platform" != "linux" && "$platform" != "darwin-x86" && "$platform" != "darwin-arm64" ]]; then
  echo "Incorrect 'platform' argument '$platform'; must be one of linux, darwin-x86, darwin-arm64"
  exit 1
fi

version="$2"
if [ -z "$version" ]; then
  echo "Missing required 'version' argument (eg. v1.2.3)"
  exit 1
fi

script_dir=$(cd "$(dirname "$0")"; pwd)
pkg_dir="$script_dir/abra"

if [ -d "$pkg_dir" ]; then
  rm -rf "$pkg_dir"
fi
mkdir "$pkg_dir"

abra build -o compiler ./src/compiler.test.abra
cp ./._abra/compiler "$pkg_dir/compiler"

cp "$script_dir/abraw" "$pkg_dir/abra"
echo -n "$version" > "$pkg_dir/version"

mkdir "$pkg_dir/include"
cp "$script_dir/../../abra_llvm/ext/libgc/lib/libgc.a" "$pkg_dir/include/."

cp -r "$script_dir/../../abra_core/std" "$pkg_dir/std"

cd "$pkg_dir"
tarfile="abra-$platform.tar.gz"
tar -czvf "$tarfile" *
mv "$pkg_dir/$tarfile" ..
