@echo off

SETLOCAL EnableDelayedExpansion

echo "Moving core files..."

set CONFIG_PATH=%UserProfile%\AppData\Roaming

move "./build.el" "%CONFIG_PATH%\build.el"
move "./.emacs" "%CONFIG_PATH%\.emacs"

mkdir "%CONFIG_PATH%/.emacs.d"
mkdir "%CONFIG_PATH%/.emacs.jcs"
robocopy /e "./.emacs.d" "%CONFIG_PATH%/.emacs.d"
robocopy /e "./.emacs.jcs" "%CONFIG_PATH%/.emacs.jcs"

echo "Attempting startup..."

set L_1="(let ((debug-on-error (>= emacs-major-version 26))
set L_2=       (url-show-status nil)
set L_3=       (user-emacs-directory default-directory)
set L_4=       (user-init-file (expand-file-name \"~/build.el\"))
set L_5=       (load-path (delq default-directory load-path)))
set L_6=         (load-file user-init-file)
set L_7=         (run-hooks (quote after-init-hook))
set L_8=         (run-hooks (quote emacs-startup-hook)))"

emacs -nw --batch --eval %L_1%%L_2%%L_3%%L_4%%L_5%%L_6%%L_7%%L_8%

echo "Startup successful"
