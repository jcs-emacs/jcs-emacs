@echo off

SETLOCAL EnableDelayedExpansion

echo "Copying core files..."

set CONFIG_PATH=%UserProfile%

echo %CONFIG_PATH%

mkdir "%CONFIG_PATH%/.emacs.d"
robocopy /e "../jcs-emacs" "%CONFIG_PATH%/.emacs.d"

echo "Done copying configuration files"
