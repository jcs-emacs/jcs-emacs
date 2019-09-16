;;; .emacs --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.
;;
;; I barely know how to program LISP, and I know
;; even less about ELISP.  So take everything in
;; this file with a grain of salt!
;;
;; - JenChieh

;;; Code:


;; DEBUG: Debug mode?
;; Produce backtraces when errors occur.
(setq debug-on-error t)


(defconst jcs-init-gc-cons-threshold (* 1024 1024 128)
  "The `GC' threshold during starting up.")
(defconst jcs-normal-gc-cons-threshold (* 1024 1024 20)
  "The `GC' threshold during the normal task.")

(defun jcs-gc-cons-threshold (speedup)
  "Set the `gc-cons-threshold' depends if is SPEEDUP."
  (if speedup
      (setq gc-cons-threshold jcs-init-gc-cons-threshold)
    (setq gc-cons-threshold jcs-normal-gc-cons-threshold)))

;; NOTE: Raise the `GC' threshold when starting Emacs.
(jcs-gc-cons-threshold t)


;;; NOTE: Set custom file.
(setq-default custom-file (expand-file-name ".jcs-custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


(defconst jcs-file-name-handler-alist file-name-handler-alist
  "Record file name handler alist.")

(setq file-name-handler-alist nil)


(defconst jcs-emacs-version-number "5.8.5"
  "JCS-Emacs version.")

;;;###autoload
(defun jcs-emacs-version ()
  "Show JCS-Emacs version info."
  (interactive)
  (message "JCS-Emacs %s" jcs-emacs-version-number))


(defun jcs-reload-emacs-reloading-p ()
  "Check if Emacs reloading now."
  (if (boundp 'reload-emacs-reloading) reload-emacs-reloading nil))

;;========================================
;;      JENCHIEH FILE LOADING
;;----------------------------------

(unless (jcs-reload-emacs-reloading-p)
  (add-to-list 'load-path "~/.emacs.jcs/")
  (add-to-list 'load-path "~/.emacs.jcs/func/")
  (add-to-list 'load-path "~/.emacs.jcs/mode/"))

;;; Auto install list of packages I want at the startup of Emacs.
(require 'jcs-package)  ;; Get the list of package dependencies.

;; Install all packages that this config needs.
(jcs-ensure-package-installed jcs-package-install-list
                              (boundp 'jcs-build-test))

(jcs-ensure-manual-package-installed jcs-package-manually-install-list
                                     (boundp 'jcs-build-test))

(defconst jcs-package-init-time (emacs-init-time)
  "Record down the package initialize time.")

;;; Utilities
(require 'jcs-log)
(require 'jcs-function)

;; ;;; Environment
(require 'jcs-file)
(require 'jcs-dev)
(require 'jcs-env)
(require 'jcs-theme)
(require 'jcs-plugin)

;; ;;; Customize
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
