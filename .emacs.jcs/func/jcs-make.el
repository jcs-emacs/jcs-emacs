;;; jcs-make.el --- Makfile related  -*- lexical-binding: t -*-
;;; Commentary: Functions for Makefile.
;;; Code:

(require 'jcs-python)

;;;###autoload
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

;;;###autoload
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


;;;###autoload
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


;;;###autoload
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

;;;###autoload
(defun jcs-makefile-newline ()
  "Makefile newline."
  (interactive)
  (insert "\n")
  (py-indent-line-outmost))

(provide 'jcs-make)
;;; jcs-make.el ends here
