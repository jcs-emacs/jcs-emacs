;;; jcs-preproc.el --- Preprocessor related  -*- lexical-binding: t -*-
;;; Commentary: When editing the TypeScript related file.
;;; Code:

;;
;; (@* "Faces" )
;;

(defconst jcs-preproc-modes
  '(cc-mode c-mode c++-mode csharp-mode objc-mode masm-mode nasm-mode)
  "List of preprocessor `major-mode'.")

(defun jcs-init-preproc-faces ()
  "Initialize preprocessor mode faces highlihgting."
  (let ((case-fold-search t))
    (dolist (mode jcs-preproc-modes)
      (font-lock-add-keywords
       mode
       '(("#pragma[ \t]+\\(comment\\)" 1 'jcs-preproc-comment-face t)
         ("#pragma[ \t]+comment[ \t]*([ \t]*\\([a-z-A-Z]+\\)[,)]" 1 'jcs-preproc-comment-type-face t))
       'end))))

(provide 'jcs-preproc)
;;; jcs-preproc.el ends here
