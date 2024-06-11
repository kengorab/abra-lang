#!/usr/bin/env bash
set -e # exit on non-zero exit status

cd ext
wget https://c9x.me/compile/release/qbe-1.2.tar.xz
tar -xf qbe-1.2.tar.xz
cd qbe-1.2
make
cd ..
mv qbe-1.2/qbe .
rm -rf qbe-1.2
rm qbe-1.2.tar.xz