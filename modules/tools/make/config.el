;;; tools/make/config.el  -*- lexical-binding: t; -*-

(require 'python-mode)

(file-header-defsrc jcs-ask-makefile-language "Major language for this Makfile: "
  '(("Default (empty)" . "Empty file")
    ("Assembly"        . "Makefile for assemly languages")
    ("C"               . "Makefile for C")
    ("C++"             . "Makefile for C++")
    ("Java"            . "Makefile for Java")
    ("Python"          . "Makefile for Python"))
  (pcase index
    (0 )  ; Does nothing
    ((or 1 2 3) (call-interactively #'jcs-ask-makefile-cc-template))
    (4 (call-interactively #'jcs-ask-makefile-java-template))
    (5 (call-interactively #'jcs-ask-makefile-python-template))))

(file-header-defsrc jcs-ask-makefile-cc-template "Type of makefile: "
  '((".."          . "Go back and re-select")
    ("Application" . "Want to create application?")
    ("Library"     . "Want to create library?"))
  (pcase index
    (0 (call-interactively #'jcs-ask-makefile-language))
    (1 (jcs-insert-makefile-cc-app-template))
    (2 (jcs-insert-makefile-cc-lib-template))))

(file-header-defsrc jcs-ask-makefile-java-template "Type of makefile: "
  '((".."          . "Go back and re-select")
    ("Application" . "Want to create application?")
    ("Library"     . "Want to create library?"))
  (pcase index
    (0 (call-interactively #'jcs-ask-makefile-language))
    (1 (jcs-insert-makefile-java-app-template))
    (2 (jcs-insert-makefile-java-lib-template))))

(file-header-defsrc jcs-ask-makefile-python-template "Type of makefile: "
  '((".."          . "Go back and re-select")
    ("Application" . "Want to create application?")
    ("Library"     . "Want to create library?"))
  (pcase index
    (0 (call-interactively #'jcs-ask-makefile-language))
    (1 (jcs-insert-makefile-python-app-template))
    (2 (jcs-insert-makefile-python-lib-template))))

(defun jcs-makefile-newline ()
  "Newline key for `makefile-mode'."
  (interactive)
  (insert "\n")
  (py-indent-line-outmost))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-makefile-cc-app-template "makefile" "cc/app.txt"
  "Default makefile template for normal application.")

(file-header-defins jcs-insert-makefile-cc-lib-template "makefile" "cc/lib.txt"
  "Library makefile template for static library or shared library.")

(file-header-defins jcs-insert-makefile-java-app-template "makefile" "java/app.txt"
  "Template for makefile Java application.")

(file-header-defins jcs-insert-makefile-java-lib-template "makefile" "java/lib.txt"
  "Template for makefile Java library.")

(file-header-defins jcs-insert-makefile-python-app-template "makefile" "python/app.txt"
  "Template for makefile Python application.")

(file-header-defins jcs-insert-makefile-python-lib-template "makefile" "python/lib.txt"
  "Template for makefile Python library.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'makefile-mode-hook
  (setq-local indent-tabs-mode t)

  (company-makefile-init)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]makefile"
                                "[Mm]akefile"
                                "[.]mak")
                              'jcs-ask-makefile-language
                              :interactive t)

  (jcs-key-local
    `(((kbd "RET") . jcs-makefile-newline))))

(jcs-add-hook 'makefile-gmake-mode-hook
  ;; XXX: Don't use gmake version, you cannot bind C-c for some reason...
  (makefile-mode))
