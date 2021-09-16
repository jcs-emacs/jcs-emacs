@echo off

SETLOCAL EnableDelayedExpansion

echo "Copying core files..."

set CONFIG_PATH=%UserProfile%\AppData\Roaming\

move "./build.el" "%CONFIG_PATH%\build.el"
move "./.emacs" "%CONFIG_PATH%\.emacs"

mkdir "%CONFIG_PATH%/.emacs.d"
mkdir "%CONFIG_PATH%/.emacs.jcs"
robocopy /e "./.emacs.d" "%CONFIG_PATH%/.emacs.d"
robocopy /e "./.emacs.jcs" "%CONFIG_PATH%/.emacs.jcs"

echo "Done copying configuration files"
