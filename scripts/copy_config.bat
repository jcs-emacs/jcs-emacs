@echo off

SETLOCAL EnableDelayedExpansion

echo "Copying core files..."

set CONFIG_PATH=%UserProfile%\AppData\Roaming\

echo %CONFIG_PATH%

move "./build.el" "%CONFIG_PATH%"
move "./.emacs" "%CONFIG_PATH%"

mkdir "%CONFIG_PATH%/.emacs.d"
mkdir "%CONFIG_PATH%/.emacs.jcs"
robocopy /e "./.emacs.d" "%CONFIG_PATH%/.emacs.d"
robocopy /e "./.emacs.jcs" "%CONFIG_PATH%/.emacs.jcs"

echo %CONFIG_PATH%

dir %CONFIG_PATH%

echo "Done copying configuration files"
