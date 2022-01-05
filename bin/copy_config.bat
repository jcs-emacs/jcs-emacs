@echo off

SETLOCAL EnableDelayedExpansion

echo "Copying core files..."

set CONFIG_PATH=%UserProfile%

echo %CONFIG_PATH%

mkdir "%CONFIG_PATH%/.emacs.d"
robocopy /e "./.emacs.d" "%CONFIG_PATH%/.emacs.d"
robocopy /e "./.emacs.jcs" "%CONFIG_PATH%/.emacs.jcs"

echo "Done copying configuration files"
