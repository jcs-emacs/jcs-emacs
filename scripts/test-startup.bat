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



set L_01="(progn
set L_02=(require (quote url-vars))
set L_03=(let ((debug-on-error (>= emacs-major-version 26))
set L_04=      (url-show-status nil)
set L_05=      (user-emacs-directory default-directory)
set L_06=      (user-init-file (expand-file-name \"~/build.el\"))
set L_07=      (load-path (delq default-directory load-path)))
set L_08=   (load-file user-init-file)
set L_09=   (run-hooks (quote after-init-hook))
set L_10=   (run-hooks (quote emacs-startup-hook))
set L_11=   (jcs-emacs-version)))"


emacs -nw --batch --eval %L_01%%L_02%%L_03%%L_04%%L_05%%L_06%%L_07%%L_08%%L_09%%L_10%%L_11%

echo "Startup successful"
