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
  (pcase lan
    ("Default (empty)")  ; Does nothing
    ("Assembly" (call-interactively 'jcs-ask-makefile-cc-template))
    ("C" (call-interactively 'jcs-ask-makefile-cc-template))
    ("C++" (call-interactively 'jcs-ask-makefile-cc-template))
    ("Java" (call-interactively 'jcs-ask-makefile-java-template))
    ("Python" (call-interactively 'jcs-ask-makefile-python-template))))

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
;; (@* "Hook" )
;;

(defun jcs-makefile-mode-hook ()
  "Makefile mode hook."
  (electric-pair-mode nil)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]makefile"
                                "[Mm]akefile"
                                "[.]mak")
                              'jcs-makefile-format-info)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))
  (jcs-bind-key (kbd "RET") #'jcs-makefile-newline)
  (jcs-bind-key (kbd "C-v") #'yank))

(add-hook 'makefile-mode-hook 'jcs-makefile-mode-hook)

(provide 'jcs-make-mode)
;;; jcs-make-mode.el ends here
