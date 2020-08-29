#!/bin/sh -e

echo "Moving core files..."

mv -f ./build.el ~/build.el
mv -f ./.emacs ~/.emacs
mv -f ./.emacs.d ~/.emacs.d
mv -f ./.emacs.jcs ~/.emacs.jcs

echo "Attempting startup..."

echo .
emacs --version
echo .

${EMACS:=emacs} -nw --batch \
                --eval '(let ((debug-on-error (>=  emacs-major-version 26))
                              (url-show-status nil)
                              (user-emacs-directory default-directory)
                              (user-init-file (expand-file-name "~/build.el"))
                              (load-path (delq default-directory load-path)))
                           (load-file user-init-file)
                           (run-hooks (quote after-init-hook))
                           (run-hooks (quote emacs-startup-hook)))'

echo "Startup successful"
