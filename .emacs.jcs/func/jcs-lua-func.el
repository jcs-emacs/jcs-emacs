;;; jcs-lua-func.el --- Lua related.  -*- lexical-binding: t -*-
;;; Commentary: When editing the Lua related file.
;;; Code:


(defun jcs-init-lua-faces ()
  "Initialize Web mode faces highlihgting."
  (let ((tmp-lua-modes '(lua-mode)))
    (dolist (mode tmp-lua-modes)
      (font-lock-add-keywords
       mode
       '(;; NOTE: Fixed comment and string conflict.
         ("\\(--[^\"\r\n]*\\)[^\"\r\n]" 1 'font-lock-comment-face t)
         ("[^\"]\\(\"[^\"]*\"\\)[^\"]" 1 'font-lock-string-face t)
         )'end))))


(defun jcs-lua-comment-prefix-p ()
  "Check if current line is a Lua style comment prefix."
  (jcs-triple-char-comment-prefix-p "-"))

(defun jcs-lua-comment-prefix-at-current-point-p ()
  "Check if the current point is Lua style comment prefix."
  (jcs-tripple-char-comment-prefix-at-current-point-p "-"))

(defun jcs-only-lua-comment-prefix-this-line-p ()
  "Check if there is only comment in this line."
  (save-excursion
    (let ((only-comment-this-line nil))
      (when (jcs-lua-comment-prefix-p)
        (jcs-goto-first-char-in-line)
        (forward-char 1)
        (forward-char 1)
        (forward-char 1)
        (when (not (jcs-is-there-char-forward-until-end-of-line-p))
          (setq only-comment-this-line t)))
      only-comment-this-line)))


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

;;;###autoload
(defun jcs-lua-maybe-insert-codedoc ()
  "Insert common Lua document/comment string."
  ;;URL: http://lua-users.org/wiki/LuaStyleGuide
  (interactive)
  (insert "-")
  (let ((active-comment nil)
        (next-line-not-empty nil))
    (save-excursion
      (when (and
             ;; Line can only have Lua comment prefix.
             (jcs-only-lua-comment-prefix-this-line-p)
             ;; Only enable when `---' at current point.
             (jcs-lua-comment-prefix-at-current-point-p))
        (setq active-comment t))

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
        (jcs-insert-comment-style-by-current-line 1)))))


(provide 'jcs-lua-func)
;;; jcs-lua-func.el ends here
