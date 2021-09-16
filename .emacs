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

;;
;; (@* "Optimizations" )
;;

(setq file-name-handler-alist nil)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it there anyway, just in case. This increases
;; memory usage, however!
(setq inhibit-compacting-font-caches t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 1024 1024))  ; 1MB

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t)

;;
;; (@* "Security" )
;;

;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be at
;; least a little more discerning.
(setq gnutls-verify-error (not (getenv-internal "INSECURE"))
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      ;; Emacs is built with `gnutls' by default, so `tls-program' would not be
      ;; used in that case. Otherwise, people have reasons to not go with
      ;; `gnutls', we use `openssl' instead. For more details, see
      ;; https://redd.it/8sykl1
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))
;;
;; (@* "Version" )
;;

(defconst jcs-emacs-version-number "6.5.1"
  "JCS-Emacs version.")

(defun jcs-emacs-version ()
  "Show JCS-Emacs version info."
  (interactive)
  (message "JCS-Emacs %s" jcs-emacs-version-number))

;;
;; (@* "Load Core" )
;;

(add-to-list 'load-path "~/.emacs.jcs/")
(add-to-list 'load-path "~/.emacs.jcs/func/")
(add-to-list 'load-path "~/.emacs.jcs/mode/")

;;; Initialize
(require 'jcs-package)
(jcs-package-install-all)

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
