;; ========================================================================
;; $File: jcs-lua-func.el $
;; $Date: 2017-11-29 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; When editing the Lua related file.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;; Code:

(defun jcs-lua-do-doc-string ()
  "Check if should insert the doc string by checking only \
comment character on the same line."

  (let ((do-doc-string t))
    (jcs-goto-first-char-in-line)

    (while (not (jcs-is-end-of-line-p))
      (forward-char 1)
      (when (and (not (jcs-current-char-equal-p " "))
                 (not (jcs-current-char-equal-p "\t"))
                 (not (jcs-current-char-equal-p "-")))
        ;; return false.
        (setq do-doc-string nil)
        (equal do-doc-string t)))

    ;; return true.
    (equal do-doc-string t)))


(defun jcs-lua-maybe-insert-codedoc ()
  "Insert common Lua document/comment string.

URL(jenchieh): http://lua-users.org/wiki/LuaStyleGuide"
  (interactive)

  (insert "-")

  (let ((active-comment nil)
        (next-line-not-empty nil))
    (save-excursion
      (backward-char 1)
      (when (jcs-current-char-equal-p "-")
        (backward-char 1)
        (when (jcs-current-char-equal-p "-")
          (backward-char 1)
          (when (not (jcs-current-char-equal-p "-"))
            (when (jcs-lua-do-doc-string)
              (setq active-comment t)))))

      ;; check if next line empty.
      (jcs-next-line)
      (when (not (jcs-current-line-empty-p))
        (setq next-line-not-empty t)))


    (when (and (equal active-comment t)
               (equal next-line-not-empty t))
      (insert "-------------------------------------------------------------\n")
      (insert "-- \n")
      (insert "----------------------------------------------------------------\n")
      (insert "----------------------------------------------------------------")

      (jcs-smart-indent-up)
      (jcs-smart-indent-down)
      (jcs-smart-indent-down)
      (jcs-smart-indent-up)
      (jcs-smart-indent-up)
      (jcs-smart-indent-up)
      (end-of-line)

      ;; Check other comment type.
      ;; ex: param, returns, etc.
      (save-excursion
        ;; Goto the function line before insert doc string.
        (jcs-next-line)
        (jcs-next-line)
        (jcs-next-line)

        ;; insert comment doc comment string.
        (jcs-insert-comment-style-by-current-line 1)
        ))
    ))
