;; ========================================================================
;; $File: jcs-preproc-func.el $
;; $Date: 2017-12-14 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions for Preprocessor Languages.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defvar jcs-preproc-highlight-modes '(cc-mode
                                      c-mode
                                      c++-mode
                                      csharp-mode
                                      nasm-mode)
  "Modes to add preprocessor highlighting.")


(defface jcs-preproc-variable-name-face
  '((t (:foreground "#B363BE")))
  "Highlight OOP tag.")
(defvar jcs-preproc-variable-name-face 'jcs-preproc-variable-name-face)


(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("[#%][ \t]*define[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
           ("[#%][ \t]*ifdef[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
           ("[#%][ \t]*ifndef[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
           ("[#%][ \t]*elif[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
           ("[#%][ \t]*if defined[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
           ("[#%][ \t]*if !defined[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
           )'end))
      jcs-preproc-highlight-modes)
