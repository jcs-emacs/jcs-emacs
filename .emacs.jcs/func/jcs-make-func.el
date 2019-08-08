;;; jcs-make-func.el --- Makfile related.  -*- lexical-binding: t -*-
;;; Commentary: Functions for Makefile.
;;; Code:


(require 'jcs-python-func)


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

  (cond ((string= lan "Default (empty)")
         (progn
           ;; Empty..
           ))
        ((string= lan "Assembly")
         (call-interactively 'jcs-ask-makefile-cc-template))
        ((string= lan "C")
         (call-interactively 'jcs-ask-makefile-cc-template))
        ((string= lan "C++")
         (call-interactively 'jcs-ask-makefile-cc-template))
        ((string= lan "Java")
         (call-interactively 'jcs-ask-makefile-java-template))
        ((string= lan "Python")
         (call-interactively 'jcs-ask-makefile-python-template))))

;;;###autoload
(defun jcs-ask-makefile-cc-template (type)
  "Ask makefile template type in Assembly, C, C++.
TYPE: type of makefile for Assembly and C/C++."
  (interactive
   (list (completing-read
          "Type of makefile: " '(".."
                                 "Application"
                                 "Library"))))

  (cond ((string= type "..")
         (call-interactively 'jcs-ask-makefile-language))
        ((string= type "Application")
         (jcs-insert-makefile-cc-app-template))
        ((string= type "Library")
         (jcs-insert-makefile-cc-lib-template))))


;;;###autoload
(defun jcs-ask-makefile-java-template (type)
  "Ask makefile template type in Java.
TYPE: type of makefile for Java."
  (interactive
   (list (completing-read
          "Type of makefile: " '(".."
                                 "Application"
                                 "Library"))))

  (cond ((string= type "..")
         (call-interactively 'jcs-ask-makefile-language))
        ((string= type "Application")
         (jcs-insert-makefile-java-app-template))
        ((string= type "Library")
         (jcs-insert-makefile-java-lib-template))))


;;;###autoload
(defun jcs-ask-makefile-python-template (type)
  "Ask makefile template type in Python.
TYPE: type of makefile for Python."
  (interactive
   (list (completing-read
          "Type of makefile: " '(".."
                                 "Application"
                                 "Library"))))

  (cond ((string= type "..")
         (call-interactively 'jcs-ask-makefile-language))
        ((string= type "Application")
         (jcs-insert-makefile-python-app-template))
        ((string= type "Library")
         (jcs-insert-makefile-python-lib-template))))


;;;###autoload
(defun jcs-makefile-newline ()
  "Makefile newline."
  (interactive)
  (insert "\n")
  (py-indent-line-outmost))


(provide 'jcs-make-func)
;;; jcs-make-func.el ends here
