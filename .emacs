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

(defconst jcs-min-require-version "26.3"
  "Minimum required Emacs version for `JCS` configuration.")

(when (version< emacs-version jcs-min-require-version)
  (error (format "This requires Emacs %s and above!" jcs-min-require-version)))

;; DEBUG: Debug mode?
;; Produce backtraces when errors occur.
(setq debug-on-error t)

(defconst jcs-gc-cons-threshold (* 1024 1024 20)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defconst jcs-gc-cons-upper-limit (* 1024 1024 128)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defun jcs-gc-cons-threshold-speed-up (speedup)
  "Set the `gc-cons-threshold' depends on SPEEDUP."
  (if speedup
      (setq gc-cons-threshold jcs-gc-cons-upper-limit)
    (setq gc-cons-threshold jcs-gc-cons-threshold)))

;; NOTE: Raise the `GC' threshold when starting Emacs.
(jcs-gc-cons-threshold-speed-up t)

;;; NOTE: Set custom file.
(setq-default custom-file (expand-file-name ".jcs-custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defconst jcs-file-name-handler-alist file-name-handler-alist
  "Record file name handler alist.")

(setq file-name-handler-alist nil)

;;----------------------------------------------------------------------------
;;; Version

(defconst jcs-emacs-version-number "6.2.1"
  "JCS-Emacs version.")

;;;###autoload
(defun jcs-emacs-version ()
  "Show JCS-Emacs version info."
  (interactive)
  (message "JCS-Emacs %s" jcs-emacs-version-number))

;;----------------------------------------------------------------------------
;;; File Loading

(defun jcs-reload-emacs-reloading-p ()
  "Check if Emacs reloading now."
  (if (boundp 'reload-emacs-reloading) reload-emacs-reloading nil))

(unless (jcs-reload-emacs-reloading-p)
  (add-to-list 'load-path "~/.emacs.jcs/")
  (add-to-list 'load-path "~/.emacs.jcs/func/")
  (add-to-list 'load-path "~/.emacs.jcs/mode/"))

(require 'jcs-package)

(defconst jcs-auto-install-pkgs t
  "Auto install the dependencies packages.")

;; Install all packages that this config needs.
(let ((install-it (or (boundp 'jcs-build-test) jcs-auto-install-pkgs)))
  (jcs-ensure-package-installed jcs-package-install-list install-it)
  (jcs-ensure-manual-package-installed jcs-package-manually-install-list install-it))

(defconst jcs-package-init-time (emacs-init-time)
  "Record down the package initialize time.")

;;----------------------------------------------------------------------------
;;; Core

;;; Utilities
(require 'jcs-log)
(require 'jcs-function)

;;; Environment
(require 'jcs-file)
(require 'jcs-dev)
(require 'jcs-env)
(require 'jcs-theme)
(require 'jcs-plugin)

;;; Customize
(require 'jcs-template)
(require 'jcs-mode)

;;; Finalize
(require 'jcs-hook)
(require 'jcs-key)
(require 'jcs-face)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; .emacs ends here
