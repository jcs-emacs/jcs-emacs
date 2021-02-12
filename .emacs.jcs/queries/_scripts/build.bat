@echo off

cd ..
SET temp="./_temp/"

SET NAME="c-sharp"
git clone https://github.com/tree-sitter/tree-sitter-%NAME% %temp%
robocopy "%temp%queries/" "./%NAME%"
rmdir %temp% /S /Q

SET NAME="css"
git clone https://github.com/tree-sitter/tree-sitter-%NAME% %temp%
robocopy "%temp%queries/" "./%NAME%"
rmdir %temp% /S /Q

SET NAME="javascript"
git clone https://github.com/tree-sitter/tree-sitter-%NAME% %temp%
robocopy "%temp%queries/" "./%NAME%"
rmdir %temp% /S /Q
