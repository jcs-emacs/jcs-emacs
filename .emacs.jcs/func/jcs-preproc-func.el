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
       '(("\\([#%][ \t]*define\\)" 1 'font-lock-preprocessor-face t)
         ("[#%][ \t]*define[ \t]+\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
         ("[#%][ \t]*ifdef[ \t]+[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
         ("[#%][ \t]*ifndef[ \t]+[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
         ("[#%][ \t]*elif[ \t]+[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
         ("[#%][ \t]*elif[ \t]+defined[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
         ("[#%][ \t]*if[ \t]+defined[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
         ("[#%][ \t]*if[ \t]+!defined[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
         ("[#%][ \t]*if[ \t]+\\([a-zA-Z0-9-.<>?,*'`@\"=_(){}:&^%$#!~ \t]+\\)" 1 'jcs-preproc-variable-name-face t)
         ("[#%][ \t]*region[ \t]+\\([a-zA-Z0-9-.<>?,*'`@\"=_(){}:&^%$#!~ \t]+\\)" 1 'jcs-preproc-variable-name-face t))
       'end))))

(provide 'jcs-preproc-func)
;;; jcs-preproc-func.el ends here
