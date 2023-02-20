;;; init.el --- Load the full configuration  -*- lexical-binding: t -*-
;;; Commentary:

;; Author:  Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL:     https://github.com/jcs-emacs/jcs-emacs

;;
;;                      ════╦╦╦╦╗
;;                  ╔═══════╩╩╩╩╩═════╗
;;                ══╝ ╔═════════════╗ ║
;;          ════════════════════════╬═╬══╗
;;           \   ╔══╗ ╓  ╥ ╥  ╥     ║ ╚══╬════
;;            ║  ║ ═╦ ║\ ║ ║  ║     ╚════╬════
;;            ║  ╚══╝ ╨ `╜ ╚══╝          ║
;;            ║   ╔══ ╔╗╔╗ ╔═╗ ╔═╕ ╔═╕   ║
;;            ║   ╠═  ║╙╜║ ╟─╢ ║   ╚═╗   ║
;;            ║   ╚══ ╨  ╨ ╨ ╨ ╚═╛ ╘═╝   ║
;;              \═══════════╣  ╠════════/
;;                 ═════════╝  ║
;;                 ════════════╝
;;
;;                [J C S - E M A C S]
;;

;; This file bootstraps the configuration, which is divided into a number of
;; other files.
;;
;; License: BSD-2-Clause

;;; Code:

;;
;; (@* "Startup" )
;;

(when (version< emacs-version "29.1")
  (error "This requires Emacs 29.1 and above!"))

(when (featurep 'esup-child) (setq gc-cons-threshold most-positive-fixnum))

;;
;; (@* "Version" )
;;

(defconst jcs-emacs-version-no "9.0.1"
  "JCS-Emacs version.")

(defun jcs-emacs-version ()
  "Show JCS-Emacs version info."
  (interactive)
  (message "JCS-Emacs %s" jcs-emacs-version-no))

;;
;; (@* "Load Core" )
;;

(setq load-path
      (append (mapcar
               (lambda (dir) (concat user-emacs-directory dir))
               '("lisp/"
                 "lisp/lib/"))
              load-path))

;;; Initialize
(require 'jcs-package)

;;; Utilities
(require 'jcs-log)
(require 'jcs-util)
(require 'jcs-window)
(require 'jcs-function)

;;; Environment
(require 'jcs-env)
(require 'jcs-disp)
(require 'jcs-ui)

;;; Standardize
(require 'jcs-theme)
(require 'jcs-template)
(require 'jcs-project)
(require 'jcs-module)

;;; Finalize
(require 'jcs-hook)
(require 'jcs-key)

;;; Customize
(load (concat user-emacs-directory "site-lisp/config.el") t t)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
