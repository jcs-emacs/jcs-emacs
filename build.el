;;; build.el --- Test the configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst jcs-build-test t
  "Define for build testing.")

;; Start regular Emacs file.
;;(load-file (expand-file-name "~/.emacs"))

(require 'package)

(let* ((package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")))
       package-enable-at-startup package-check-signature
       (pkgs '(dash f lv ht spinner markdown-mode deferred el-mock)))
  (package-initialize)

  (mapc (lambda (pkg)
          (unless (package-installed-p pkg)
            (package-refresh-contents) (package-install pkg)))
        pkgs)

  (add-hook 'kill-emacs-hook
            `(lambda ()
               (unless (boundp 'emacs-lsp-ci)
                 (delete-directory ,user-emacs-directory t)))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; build.el ends here
