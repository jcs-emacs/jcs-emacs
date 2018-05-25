;; ========================================================================
;; $File: jcs-cc-func.el $
;; $Date: 2017-11-20 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions for C/C++ common.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;;###autoload
(defun jcs-toggle-cc-mode ()
  "Toggle c/c++ mode."
  (interactive)
  (cond ((equal major-mode 'c-mode) (progn (c++-mode)))
        ((equal major-mode 'c++-mode) (progn (c-mode)))))

;;;###autoload
(defun jcs-toggle-c-comment-style ()
  "Toggle comment style between /* */ and //."
  (interactive)

  (when (or (jcs-is-current-major-mode-p "c-mode")
            (jcs-is-current-major-mode-p "c++-mode"))
    (if (string= comment-start "// ")
        (progn
          (setq comment-start "/*"
                comment-start-skip "/\\*+[ \t]*"
                comment-end "*/"
                comment-end-skip "[ \t]*\\*+/"))
      (progn
        (setq comment-start "// "
              comment-end "")))))
