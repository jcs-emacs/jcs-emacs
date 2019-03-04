;; ========================================================================
;; $File: jcs-comment.el $
;; $Date: 2017-05-31 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;----------------------------------------------
;; Comment
;;----------------------------------------------

(defun jcs-triple-char-comment-prefix-p (in-char)
  "Check if current line is a triple IN-CHAR style comment prefix.
For instance, `///', `---', etc.  Those are all triple char style comment prefix.
IN-CHAR : input triple char."
  (save-excursion
    (let ((is-comment-prefix nil))
      (jcs-goto-first-char-in-line)
      (forward-char 1)
      (when (jcs-current-char-equal-p in-char)
        (forward-char 1)
        (when (jcs-current-char-equal-p in-char)
          (forward-char 1)
          (when (jcs-current-char-equal-p in-char)
            (setq is-comment-prefix t))))
      is-comment-prefix)))

(defun jcs-tripple-char-comment-prefix-at-current-point-p (in-char)
  "Check if the current point is triple IN-CHAR style comment prefix.
For instance, `///', `---', etc.  Those are all triple char style comment prefix.
IN-CHAR : input triple char."
  (save-excursion
    (let ((is-comment-prefix-at-point nil))
      (when (jcs-current-char-equal-p in-char)
        (backward-char 1)
        (when (jcs-current-char-equal-p in-char)
          (backward-char 1)
          (when (jcs-current-char-equal-p in-char)
            (setq is-comment-prefix-at-point t))))
      is-comment-prefix-at-point)))


(defun jcs-do-doc-string ()
  "Check if should insert the doc string by checking only \
comment character on the same line."
  (let ((do-doc-string t))
    (jcs-goto-first-char-in-line)

    (while (not (jcs-is-end-of-line-p))
      (forward-char 1)
      (when (and (not (jcs-current-char-equal-p " "))
                 (not (jcs-current-char-equal-p "\t"))
                 (not (jcs-current-char-equal-p "*"))
                 (not (jcs-current-char-equal-p "/")))
        ;; return false.
        (setq do-doc-string nil)
        do-doc-string))

    ;; return true.
    do-doc-string))

;;;###autoload
(defun jcs-smart-context-line-break ()
  "Comment block."
  (interactive)
  (let ((last nil)
        (point-beginning-of-line nil)
        (point-end-of-line nil)
        (start-of-global-comment-doc nil))

    ;; start position
    (setq last (point))

    ;; record down the beginning of the line position.
    (beginning-of-line)
    (setq point-beginning-of-line (point))

    ;; record down the end of the line position.
    (end-of-line)
    (setq point-end-of-line (point))

    ;; back to original position
    (goto-char last)

    ;; check if inside the comment block.
    (if (jcs-is-inside-comment-block-p)
        (progn
          (setq last (point))

          (setq start-of-global-comment-doc nil)

          ;; check the '/*' and '*/' on the same line?
          (if (and (search-backward "/*" point-beginning-of-line t)
                   (search-forward "*/" point-end-of-line t)
                   (jcs-do-doc-string))
              (progn
                (setq start-of-global-comment-doc t)

                (goto-char last)

                (insert "\n* ")
                (indent-for-tab-command)

                (insert "\n")
                (indent-for-tab-command)

                ;; back one line up
                (jcs-previous-line)

                ;; Insert comment string here..
                (when (or (jcs-is-current-major-mode-p "c-mode")
                          (jcs-is-current-major-mode-p "c++-mode")
                          (jcs-is-current-major-mode-p "java-mode")
                          ;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
                          ;; TODO(jenchieh): If we decide to use
                          ;; c-type docstirng. Then we need to
                          ;; uncomment the line below.
                          ;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
                          ;;(jcs-is-current-major-mode-p "csharp-mode")
                          (jcs-is-current-major-mode-p "js2-mode")
                          (jcs-is-current-major-mode-p "php-mode")
                          ;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
                          ;; TODO(jenchieh): `typescript-mode' itself
                          ;; does not have each keyword identifier.
                          ;; Meaning the programming language itself is
                          ;; quite simple and docstring will be much harder
                          ;; to implement.
                          ;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
                          ;;(jcs-is-current-major-mode-p "typescript-mode")
                          (jcs-is-current-major-mode-p "web-mode"))
                  (jcs-insert-comment-string))

                ;; goto the end of line
                (end-of-line))
            (progn
              (goto-char last)

              (newline-and-indent)
              (when (jcs-is-inside-comment-block-p)
                (insert "* "))))

          (unless start-of-global-comment-doc
            (let ((is-global-comment-doc nil))
              (save-excursion
                (jcs-goto-start-of-the-comment)
                (forward-char 1)
                (when (jcs-current-char-equal-p "*")
                  (setq is-global-comment-doc t)))

              (unless is-global-comment-doc
                (let (;; Check if the next line is the doc string
                      ;; comment line.
                      (is-next-line-doc-string-comment-line nil)
                      ;; When we break a line and there are still
                      ;; some content on the right.
                      (line-have-content-on-right nil))

                  (cond (;;; NOTE(jenchieh): CSharp-Mode
                         (jcs-is-current-major-mode-p "csharp-mode")
                         (progn
                           (save-excursion
                             (when (not (jcs-current-line-comment-p))
                               (setq line-have-content-on-right t)))

                           (save-excursion
                             (jcs-previous-line)

                             (if (not (jcs-vs-csharp-only-vs-comment-prefix-this-line-p))
                                 (progn
                                   (when (jcs-vs-csharp-comment-prefix-p)
                                     (setq is-next-line-doc-string-comment-line t)))
                               (progn
                                 (when line-have-content-on-right
                                   (setq is-next-line-doc-string-comment-line t)))))

                           ;; If we still not sure to insert docstring comment
                           ;; line yet. Then we need to do deeper check.
                           (when (not is-next-line-doc-string-comment-line)
                             (let ((prev-line-vs-prefix nil)
                                   (next-line-vs-prefix nil))
                               (save-excursion
                                 (jcs-previous-line)
                                 (when (jcs-vs-csharp-comment-prefix-p)
                                   (setq prev-line-vs-prefix t)))

                               ;; Only when previous have prefix.
                               (when prev-line-vs-prefix
                                 (save-excursion
                                   (jcs-next-line)
                                   (when (jcs-vs-csharp-comment-prefix-p)
                                     (setq next-line-vs-prefix t)))

                                 (when (and prev-line-vs-prefix
                                            next-line-vs-prefix)
                                   (setq is-next-line-doc-string-comment-line t)))))

                           ;; Is doc-string comment line. Insert
                           ;; doc-string comment.
                           (when is-next-line-doc-string-comment-line
                             (insert "/// "))))
                        (;;; NOTE(jenchieh): Lua-Mode
                         (jcs-is-current-major-mode-p "lua-mode")
                         (progn
                           ;; Just insert for Lua.
                           ;; Lua does not have issue like CSharp.
                           (insert "-- "))))
                  (indent-for-tab-command)
                  )))))
      ;; else insert new line
      (progn
        (newline-and-indent)))))


;;;###autoload
(defun jcs-c-comment-pair ()
  "Auto pair c style comment block."
  (interactive)
  (let ((insert-pair nil))
    (save-excursion
      (ignore-errors
        (when (jcs-current-char-equal-p "/")
          (setq insert-pair t))))

    (insert "*")

    (save-excursion
      (when (and
             ;; Check insert pair string?
             (equal insert-pair t)
             ;; Check new comment block?
             (equal (jcs-check-new-block-of-comment) t))
        (insert "*/")))))

(defun jcs-check-new-block-of-comment ()
  "If there is one closing comment string without opening comment \
string, do not insert closing comment string.  Check this situation."
  (let ((check-point (point))
          (new-comment-block t))
    (save-excursion

      (jcs-move-to-forward-a-char "/")
      (backward-char 1)
      (when (jcs-current-char-equal-p "*")
        (jcs-goto-start-of-the-comment)

        ;; No opening comment string by using
        ;; `jcs-goto-start-of-the-comment' function.
        (if (>= check-point (point))
            (setq new-comment-block nil))))
    new-comment-block))

(defun jcs-insert-comment-string ()
  "Insert comment document string."
  (save-excursion
    ;; Goto the function line before insert doc string.
    (jcs-next-line)
    (jcs-next-line)

    ;; insert comment doc comment string.
    (jcs-insert-comment-style-by-current-line 2)))

;;;###autoload
(defun jcs-toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  ;; SOURCE: http://stackoverflow.com/questions/9688748/emacs-comment-uncomment-current-line
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;;;###autoload
(defun jcs-comment-uncomment-region-or-line ()
  "Comment line or region, if there are region select then just comment region.
Otherwise comment line."
  (interactive)

  ;; check if there are region select
  (if (and mark-active
           (/= (point) (mark)))
      (progn

        (setq before-comment-point (point))

        (if (jcs-is-infront-first-char-at-line-p)
            (progn
              (jcs-safe-forward-char)
              (jcs-safe-forward-char)
              (jcs-safe-forward-char)))

        (if (nth 4 (syntax-ppss))
            (progn
              (goto-char before-comment-point)
              (uncomment-region (region-beginning) (region-end)))
          (progn
            (goto-char before-comment-point)
            (comment-region (region-beginning) (region-end)))))
    (progn
      ;; else we just comment one single line.
      (jcs-toggle-comment-on-line))))

;;;###autoload
(defun jcs-comment-region-or-line ()
  "If no region selected then just comment the line."
  (interactive)

  ;; check if there are region select
  (if (and mark-active
           (/= (point) (mark)))
      (progn
        (if (nth 4 (syntax-ppss))
            (progn
              ;; do not uncomment.
              )
          (comment-region (region-beginning) (region-end))))
    ;; else we just comment one single line.
    (comment-region (line-beginning-position) (line-end-position))))

;;;###autoload
(defun jcs-uncomment-region-or-line ()
  "If no region selected then just comment the line."
  (interactive)

  ;; check if there are region select
  (if (and mark-active
           (/= (point) (mark)))
      (progn
        (uncomment-region (region-beginning) (region-end))
        )
    ;; else we just comment one single line.
    (uncomment-region (line-beginning-position) (line-end-position))))
