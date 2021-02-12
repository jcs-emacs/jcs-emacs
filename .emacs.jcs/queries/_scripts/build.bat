@echo off

cd ..
SET temp="./_temp/"

SET target=("c-sharp" "css" "javascript")

FOR %%n IN %target% DO (
git clone https://github.com/tree-sitter/tree-sitter-%%n %temp%
robocopy "%temp%queries/" "./%%n"
rmdir %temp% /S /Q)
