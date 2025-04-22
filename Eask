;; -*- mode: eask; lexical-binding: t -*-

(package "jcs-emacs"
         "9.1.1"
         "Emacs configuration works across all OSs")

(website-url "https://github.com/jcs-emacs/jcs-emacs")
(keywords "lisp" "config")

(files "early-init.el" "init.el"
       "lisp/*.el"
       "lisp/lib/*.el"
       "site-lisp/*.el"
       "modules/**"
       "banners/**"
       "fonts/**"
       "templates/**")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)
(source 'jcs-elpa)

(source-priority 'melpa    5)
(source-priority 'jcs-elpa 10)

(depends-on "emacs" "30.1")

(depends-on "pkg-dm")
(depends-on "noflet")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
