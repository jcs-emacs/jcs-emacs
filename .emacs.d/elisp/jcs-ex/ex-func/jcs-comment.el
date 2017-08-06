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

;; jcs-function is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-function is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;----------------------------------------------
;; Comment
;;----------------------------------------------

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

        ;; check the '/*' and '*/' on the same line?
        (if (and (search-backward "/*" point-beginning-of-line t)
                 (search-forward "*/" point-end-of-line t))
            (progn
              (goto-char last)

              (insert "\n* ")
              (indent-for-tab-command)

              (insert "\n")
              (indent-for-tab-command)

              ;; back one line up
              (previous-line 1)

              ;; goto the end of line
              (end-of-line)
              )
          (progn
            (goto-char last)

            (insert "\n")

            (if (nth 4 (syntax-ppss))
                (progn
                  (insert "* ")
                  (indent-for-tab-command)
                  )
              )
            )
          ) ;; end (if (looking-back "/* "))
        )
    ;; else insert new line
    (progn
      (newline-and-indent)
      ))
  )


;;;###autoload
(defun jcs-c-comment-pair ()
  "Auto pair c style comment block"
  (interactive)

  (let (next-line-is-commented)

    (setq before-insert-point (point))

    ;; Prevent the line is the end of document.
    (ignore-errors (next-line 1))

    (if (nth 4 (syntax-ppss))
        ;; record down next line is comment.
        (setq next-line-is-commented t)
      )

    (goto-char before-insert-point)

    (insert "*")

    ;; record down the cursor  position after insert '*' character.
    (setq last (point))

    ;; record down the beginning of the line position.
    (beginning-of-line)
    (setq point-beginning-of-line (point))

    ;; record down the end of the line position.
    (end-of-line)
    (setq point-end-of-line (point))

    ;; back to original position
    (goto-char last)

    ;; check inside the comment block?
    (if (search-backward "/*" point-beginning-of-line t)
        (progn
          (if (not (search-forward "*/" point-end-of-line t))
              (progn
                (if (null next-line-is-commented)
                    (progn
                      ;; NOTE: next line is not a commented
                      ;; line we add the end comment block.
                      (goto-char last)
                      (insert "*/")
                      )
                  )
                )
            )
          )
      ) ;; end (if (search-forward ...))
    )

  ;; go back before searching.
  ;; because searching will mess up the cursor point.
  (goto-char last)
  )


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
        (if (nth 4 (syntax-ppss))
            (progn
              (uncomment-region (region-beginning) (region-end))
              )
          (comment-region (region-beginning) (region-end))
          )
        )
    ;; else we just comment one single line.
    (toggle-comment-on-line)
    )
  )

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
          (comment-region (region-beginning) (region-end))
          )
        )
    ;; else we just comment one single line.
    (comment-region (line-beginning-position) (line-end-position))
    )
  )

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
    )
  )


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-comment.el file
