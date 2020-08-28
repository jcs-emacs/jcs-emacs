@echo off

echo "Moving core files..."

move "./build.el" "%UserProfile%/build.el"
move "./.emacs" "%UserProfile%/.emacs"
move "./.emacs.d" "%UserProfile%"
move "./.emacs.jcs" "%UserProfile%"

echo "Attempting startup..."

emacs -nw --batch \
      --eval '(let ((debug-on-error (>=  emacs-major-version 26))
                    (url-show-status nil)
                    (user-emacs-directory default-directory)
                    (user-init-file (expand-file-name "~/build.el"))
                    (load-path (delq default-directory load-path)))
                      (load-file user-init-file)
                      (run-hooks (quote after-init-hook))
                      (run-hooks (quote emacs-startup-hook)))'

echo "Startup successful"
