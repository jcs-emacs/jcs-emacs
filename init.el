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

(defconst jcs-required-emacs-version "30.1"
  "Minimum required Emacs version for this configuration.")

(when (version< emacs-version jcs-required-emacs-version)
  (error "This requires Emacs %s and above!" jcs-required-emacs-version))

;;
;; (@* "Version" )
;;

(defconst jcs-emacs-version-no "9.2.1"
  "JCS-Emacs version.")

(defun jcs-emacs-version ()
  "Show JCS-Emacs version info."
  (interactive)
  (message "JCS-Emacs %s" jcs-emacs-version-no))

;;
;; (@* "Constant" )
;;

(defconst jcs-homepage
  "https://github.com/jcs-emacs/jcs-emacs"
  "The Github page of JCS-Emacs.")

;;
;; (@* "Core" )
;;

(when (featurep 'esup-child) (setq gc-cons-threshold most-positive-fixnum))

(setq load-path
      (append (mapcar
               (lambda (dir) (concat user-emacs-directory dir))
               '("lisp/"
                 "lisp/lib/"
                 "site-lisp/"))
              load-path))

;;; Initialize
(require 'jcs-package)

;;; Utilities
(require 'jcs-util)
(require 'jcs-window)
(require 'jcs-function)

;;; Environment
(require 'jcs-env)
(require 'jcs-disp)
(require 'jcs-ui)

;;; Standardize
(require 'jcs-theme)
(require 'jcs-project)
(require 'jcs-module)

;;; Finalize
(require 'jcs-hook)
(require 'jcs-key)

;;; Customize
(load (expand-file-name "~/.emacs.jcs.d/init.el") t t)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
