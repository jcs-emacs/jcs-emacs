;;; jcs-preproc-func.el --- Preprocessor Languages related.  -*- lexical-binding: t -*-
;;; Commentary: Functions for Preprocessor Languages.
;;; Code:


(defun jcs-init-preproc-faces ()
  "Initialize preprocessor faces highlihgting."
  (let ((preproc-highlight-modes '(cc-mode
                                   c-mode
                                   c++-mode
                                   csharp-mode
                                   nasm-mode)))
    (dolist (mode preproc-highlight-modes)
      (font-lock-add-keywords
       mode
       '(("[#%][ \t]*define[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
         ("[#%][ \t]*ifdef[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
         ("[#%][ \t]*ifndef[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
         ("[#%][ \t]*elif[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
         ("[#%][ \t]*if defined[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
         ("[#%][ \t]*if !defined[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
         ("[#%][ \t]*if\\([a-zA-Z0-9-.<>?,*'`@\"=_(){}:&^%$#!~ \t]+\\)" 1 'jcs-preproc-variable-name-face t)
         ("[#%][ \t]*region\\([a-zA-Z0-9-.<>?,*'`@\"=_(){}:&^%$#!~ \t]+\\)" 1 'jcs-preproc-variable-name-face t)
         )'end))))


(provide 'jcs-preproc-func)
;;; jcs-preproc-func.el ends here
