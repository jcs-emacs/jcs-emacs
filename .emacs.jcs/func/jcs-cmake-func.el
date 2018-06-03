;; ========================================================================
;; $File: jcs-cmake-func.el $
;; $Date: 2017-12-04 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions for CMake and Makefile.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun jcs-ask-makefile-language (lan)
  "Ask makefile what major language is this makefile going to use.
Then specialize makefile to that target language.

LAN : temporary variable store user langauge input."

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
         (progn
           (call-interactively 'jcs-ask-makefile-cc-template)
           ))
        ((string= lan "C")
         (progn
           (call-interactively 'jcs-ask-makefile-cc-template)
           ))
        ((string= lan "C++")
         (progn
           (call-interactively 'jcs-ask-makefile-cc-template)
           ))
        ((string= lan "Java")
         (progn
           (call-interactively 'jcs-ask-makefile-java-template)
           ))
        ((string= lan "Python")
         (progn
           (call-interactively 'jcs-ask-makefile-python-template)
           ))
        )
  )

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
         (progn
           (call-interactively 'jcs-ask-makefile-language)
           ))
        ((string= type "Application")
         (progn
           (jcs-insert-makefile-cc-app-template)))
        ((string= type "Library")
         (progn
           (jcs-insert-makefile-cc-lib-template)))
        ))


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
         (progn
           (call-interactively 'jcs-ask-makefile-language)
           ))
        ((string= type "Application")
         (progn
           (jcs-insert-makefile-java-app-template)))
        ((string= type "Library")
         (progn
           (jcs-insert-makefile-java-lib-template)))
        ))


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
         (progn
           (call-interactively 'jcs-ask-makefile-language)
           ))
        ((string= type "Application")
         (progn
           (jcs-insert-makefile-python-app-template)))
        ((string= type "Library")
         (progn
           (jcs-insert-makefile-python-lib-template)))
        ))


;;;###autoload
(defun jcs-makefile-newline ()
  "Newline"
  (interactive)
  (insert "\n")
  (py-indent-line-outmost))
