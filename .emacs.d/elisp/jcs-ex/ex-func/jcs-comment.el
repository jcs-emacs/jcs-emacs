;; This is the start of jcs-comment.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-comment.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2017>
;; Time-stamp: <2017-05-31 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-comment is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-comment is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;----------------------------------------------
;; Comment
;;----------------------------------------------

(defun jcs-do-doc-string ()
  "Check if should insert the doc string by checking only \
comment character on the same line."

  (let ((do-doc-string t))
    (jcs-goto-first-char-in-line)

    (while (not (is-end-of-line-p))
      (forward-char 1)
      (when (and (not (current-char-equal-p " "))
                 (not (current-char-equal-p "\t"))
                 (not (current-char-equal-p "*"))
                 (not (current-char-equal-p "/")))
        ;; return false.
        (setq do-doc-string nil)
        (equal do-doc-string t)))

    ;; return true.
    (equal do-doc-string t)))

;;;###autoload
(defun jcs-smart-context-line-break ()
  "Comment block."
  (interactive)

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
  (if (and (nth 4 (syntax-ppss)))
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
              (previous-line 1)

              ;; Insert comment string here..
              (if (or (jcs-is-current-major-mode-p "c-mode")
                      (jcs-is-current-major-mode-p "c++-mode")
                      (jcs-is-current-major-mode-p "java-mode")
                      ;;(jcs-is-current-major-mode-p "csharp-mode")
                      (jcs-is-current-major-mode-p "js2-mode")
                      (jcs-is-current-major-mode-p "php-mode")
                      (jcs-is-current-major-mode-p "web-mode"))
                  (jcs-insert-comment-string))

              ;; goto the end of line
              (end-of-line)
              )
          (progn
            (goto-char last)

            (insert "\n")
            (when (is-inside-comment-block-p)
              (insert "* "))

            (indent-for-tab-command)))

        (when (equal start-of-global-comment-doc nil)
          (let ((is-global-comment-doc nil))
            (save-excursion
              (jcs-goto-start-of-the-comment)
              (forward-char 1)
              (when (current-char-equal-p "*")
                (setq is-global-comment-doc t)))

            (when (equal is-global-comment-doc nil)
              (call-interactively 'jcs-backward-kill-line)

              (cond ((jcs-is-current-major-mode-p "csharp-mode")
                     (progn
                       (insert "/// ")
                       ))
                    ((jcs-is-current-major-mode-p "lua-mode")
                     (progn
                       (insert "-- "))))
              (indent-for-tab-command)
              )))
        )
    ;; else insert new line
    (progn
      (newline-and-indent))
    ))


;;;###autoload
(defun jcs-c-comment-pair ()
  "Auto pair c style comment block."
  (interactive)

  (let ((insert-pair nil))

    (save-excursion
      (ignore-errors
        (when (current-char-equal-p "/")
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
      (when (current-char-equal-p "*")
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
(defun toggle-comment-on-line ()
  "comment or uncomment current line.
SOURCE: http://stackoverflow.com/questions/9688748/emacs-comment-uncomment-current-line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;;;###autoload
(defun jcs-comment-uncomment-region-or-line ()
  "Comment line or region, if there are region select then just
comment region. Otherwise comment line."
  (interactive)

  ;; check if there are region select
  (if (and mark-active
           (/= (point) (mark)))
      (progn

        (setq before-comment-point (point))

        (if (is-met-first-char-at-line-p)
            (progn
              (safe-forward-char)
              (safe-forward-char)
              (safe-forward-char)))

        (if (nth 4 (syntax-ppss))
            (progn
              (goto-char before-comment-point)
              (uncomment-region (region-beginning) (region-end)))
          (progn
            (goto-char before-comment-point)
            (comment-region (region-beginning) (region-end)))))
    (progn
      ;; else we just comment one single line.
      (toggle-comment-on-line))))

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
    (uncomment-region (line-beginning-position) (line-end-position))
    ))


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-comment.el file
