;;; jcs-make-mode.el --- Makefile mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'make-mode)

(defun jcs-ask-makefile-language (lan)
  "Ask make file language LAN type."
  (interactive
   (list (completing-read
          "Major language for this Makfile: " '("Default (empty)"
                                                "Assembly"
                                                "C"
                                                "C++"
                                                "Java"
                                                "Python"))))
  (when-let
      ((fnc
        (pcase lan
          ("Default (empty)")  ; Does nothing
          ("Assembly" #'jcs-ask-makefile-cc-template)
          ("C" #'jcs-ask-makefile-cc-template)
          ("C++" #'jcs-ask-makefile-cc-template)
          ("Java" #'jcs-ask-makefile-java-template)
          ("Python" #'jcs-ask-makefile-python-template))))
    (call-interactively fnc)))

(defun jcs-ask-makefile-cc-template (type)
  "Ask makefile template type in Assembly, C, C++.
TYPE: type of makefile for Assembly and C/C++."
  (interactive
   (list (completing-read
          "Type of makefile: " '(".."
                                 "Application"
                                 "Library"))))
  (pcase type
    (".." (call-interactively 'jcs-ask-makefile-language))
    ("Application" (jcs-insert-makefile-cc-app-template))
    ("Library" (jcs-insert-makefile-cc-lib-template))))


(defun jcs-ask-makefile-java-template (type)
  "Ask makefile template type in Java.
TYPE: type of makefile for Java."
  (interactive
   (list (completing-read
          "Type of makefile: " '(".."
                                 "Application"
                                 "Library"))))
  (pcase type
    (".." (call-interactively 'jcs-ask-makefile-language))
    ("Application" (jcs-insert-makefile-java-app-template))
    ("Library" (jcs-insert-makefile-java-lib-template))))


(defun jcs-ask-makefile-python-template (type)
  "Ask makefile template type in Python.
TYPE: type of makefile for Python."
  (interactive
   (list (completing-read
          "Type of makefile: " '(".."
                                 "Application"
                                 "Library"))))
  (pcase type
    (".." (call-interactively 'jcs-ask-makefile-language))
    ("Application" (jcs-insert-makefile-python-app-template))
    ("Library" (jcs-insert-makefile-python-lib-template))))

(defun jcs-makefile-newline ()
  "Newline key for `makefile-mode'."
  (interactive)
  (insert "\n")
  (py-indent-line-outmost))

;;
;; (@* "Templates" )
;;

(defun jcs-makefile-format-info ()
  "File header format specific for makefile depends on language selected."
  (call-interactively 'jcs-ask-makefile-language))

(defun jcs-insert-makefile-cc-app-template ()
  "Default makefile template for normal application."
  (jcs--file-header--insert "makefile" "cc/app.txt"))

(defun jcs-insert-makefile-cc-lib-template ()
  "Library makefile template for static library or shared library."
  (jcs--file-header--insert "makefile" "cc/lib.txt"))

(defun jcs-insert-makefile-java-app-template ()
  "Template for makefile Java application."
  (jcs--file-header--insert "makefile" "java/app.txt"))

(defun jcs-insert-makefile-java-lib-template ()
  "Template for makefile Java library."
  (jcs--file-header--insert "makefile" "java/lib.txt"))

(defun jcs-insert-makefile-python-app-template ()
  "Template for makefile Python application."
  (jcs--file-header--insert "makefile" "python/app.txt"))

(defun jcs-insert-makefile-python-lib-template ()
  "Template for makefile Python library."
  (jcs--file-header--insert "makefile" "python/lib.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'makefile-mode-hook
  (electric-pair-mode nil)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]makefile"
                                "[Mm]akefile"
                                "[.]mak")
                              'jcs-makefile-format-info)

  (jcs-key-local
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next))
      ((kbd "RET")    . jcs-makefile-newline)
      ((kbd "C-v")    . yank))))

(provide 'jcs-make-mode)
;;; jcs-make-mode.el ends here
