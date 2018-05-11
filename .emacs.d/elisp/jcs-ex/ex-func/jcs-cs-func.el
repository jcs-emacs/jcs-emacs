;; ========================================================================
;; $File: jcs-cs-func.el $
;; $Date: 2017-11-28 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; When editing the C# related file.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun jcs-vs-csharp-do-doc-string ()
  "Check if should insert the doc string by checking only \
comment character on the same line."

  (let ((do-doc-string t))
    (jcs-goto-first-char-in-line)

    (while (not (is-end-of-line-p))
      (forward-char 1)
      (when (and (not (current-char-equal-p " "))
                 (not (current-char-equal-p "\t"))
                 (not (current-char-equal-p "/")))
        ;; return false.
        (setq do-doc-string nil)
        (equal do-doc-string t)))

    ;; return true.
    (equal do-doc-string t)))

(defun jcs-vs-csharp-maybe-insert-codedoc ()
  "Insert comment like Visual Studio comment style.

URL(jenchieh): https://github.com/josteink/csharp-mode/issues/123"
  (interactive)

  (insert "/")

  (let ((active-comment nil)
        (next-line-not-empty nil))
    (save-excursion
      (backward-char 1)
      (when (current-char-equal-p "/")
        (backward-char 1)
        (when (current-char-equal-p "/")
          (backward-char 1)
          (when (not (current-char-equal-p "/"))
            (when (jcs-vs-csharp-do-doc-string)
              (setq active-comment t)))))

      ;; check if next line empty.
      (jcs-next-line)
      (when (not (current-line-empty-p))
        (setq next-line-not-empty t)))


    (when (and (equal active-comment t)
               (equal next-line-not-empty t))
      (insert " <summary>\n")
      (insert "/// \n")
      (insert "/// </summary>")

      (jcs-smart-indent-up)
      (jcs-smart-indent-down)
      (jcs-smart-indent-up)
      (end-of-line)

      ;; Check other comment type.
      ;; ex: param, returns, etc.
      (save-excursion
        ;; Goto the function line before insert doc string.
        (jcs-next-line)
        (jcs-next-line)

        ;; insert comment doc comment string.
        (jcs-insert-comment-style-by-current-line 2)
        ))
    ))
