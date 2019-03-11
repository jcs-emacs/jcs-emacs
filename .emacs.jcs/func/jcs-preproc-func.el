;;; jcs-preproc-func.el --- Preprocessor Languages related.  -*- lexical-binding: t -*-
;;; Commentary: Functions for Preprocessor Languages.
;;; Code:


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


(provide 'jcs-preproc-func)
;;; jcs-preproc-func.el ends here
