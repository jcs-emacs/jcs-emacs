#!/bin/sh

cd ..
temp="./_temp/"

target=("c-sharp" "css" "javascript" "typescript")

for out in "${target[@]}"
do
    git clone https://github.com/tree-sitter/tree-sitter-${out} ${temp}
    cp -R "${temp}queries/" "./${out}"
    rm -rf ${temp}
done
