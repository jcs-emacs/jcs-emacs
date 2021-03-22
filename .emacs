;;; .emacs --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Author:  Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL:     https://github.com/jcs090218/jcs-emacs

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
;;             \            _  _         /
;;              \═══════════╣  ╠════════/
;;                 ═════════╝  ║
;;                 ════════════╝
;;
;;                [J C S - E M A C S]
;;

;; This file bootstraps the configuration, which is divided into
;; a number of other files.
;;
;; I barely know how to program LISP, and I know even less about ELISP.
;; So take everything in this file with a grain of salt!

;;; Code:

;;
;; (@* "Startup" )
;;

(defconst jcs-min-require-version "27.1"
  "Minimum required Emacs version for `JCS` configuration.")

(when (version< emacs-version jcs-min-require-version)
  (error (format "This requires Emacs %s and above!" jcs-min-require-version)))

(defconst jcs-gc-cons-threshold (* 1024 1024 20)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defconst jcs-gc-cons-upper-limit (* 1024 1024 128)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defun jcs-gc-cons-threshold-speed-up (speedup)
  "Set the `gc-cons-threshold' depends on SPEEDUP."
  (setq gc-cons-threshold (if speedup jcs-gc-cons-upper-limit jcs-gc-cons-threshold)))

;; NOTE: Raise the `GC' threshold when starting Emacs.
(jcs-gc-cons-threshold-speed-up t)

;;; NOTE: Set custom file.
(setq-default custom-file (expand-file-name ".jcs-custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defconst jcs-file-name-handler-alist file-name-handler-alist
  "Record file name handler alist.")

(setq file-name-handler-alist nil)

;;
;; (@* "Version" )
;;

(defconst jcs-emacs-version-number "6.4.3"
  "JCS-Emacs version.")

;;;###autoload
(defun jcs-emacs-version ()
  "Show JCS-Emacs version info."
  (interactive)
  (message "JCS-Emacs %s" jcs-emacs-version-number))

;;
;; (@* "File Loading" )
;;

(add-to-list 'load-path "~/.emacs.jcs/")
(add-to-list 'load-path "~/.emacs.jcs/func/")
(add-to-list 'load-path "~/.emacs.jcs/mode/")

(require 'jcs-package)
(jcs-package-install-all)

;;
;; (@* "Core" )
;;

;;; Utilities
(require 'jcs-log)
(require 'jcs-function)

;;; Environment
(require 'jcs-file)
(require 'jcs-dev)
(require 'jcs-env)
(require 'jcs-theme)
(require 'jcs-plugin)

;;; Standardize
(require 'jcs-template)
(require 'jcs-mode)
(require 'jcs-project)

;;; Finalize
(require 'jcs-minibuf)
(require 'jcs-hook)
(require 'jcs-key)
(require 'jcs-face)

;;; Customize
(require 'jcs-config)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; .emacs ends here
