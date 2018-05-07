;; ========================================================================
;; $File: jcs-nasm-func.el $
;; $Date: 2017-12-31 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;; First load the mode to prevent overwrite after.
(require 'nasm-mode)

(defun nasm-indent-line ()
  "Indent current line as NASM assembly code."
  (interactive)
  (let ((orig (- (point-max) (point))))
    (back-to-indentation)
    (if (or (looking-at (nasm--opt nasm-directives))
            (looking-at (nasm--opt nasm-pp-directives))
            (looking-at "\\[")
            (looking-at ";;+")
            ;;--------------------------------------------------
            ;; ATTENTION(jenchieh): Self copy this function from
            ;; source code. Add these rules for my own use.
            (jcs-is-nasm-indent)
            ;;--------------------------------------------------
            (looking-at nasm-label-regexp))
        (indent-line-to 0)
      (indent-line-to nasm-basic-offset))
    (when (> (- (point-max) orig) (point))
      (goto-char (- (point-max) orig)))))

(defun jcs-is-nasm-indent ()
  "JayCeS's own indent nasm rules.
@return boolean : true - do indent, false - vice versa."
  (let ((do-indent nil))
    (save-excursion
      ;; Goto the first character of current line.
      (back-to-indentation-or-beginning)
      (when (is-beginning-of-line-p)
        (back-to-indentation-or-beginning))
      (forward-char 1)

      ;; Check rule here..
      (when (current-char-equal-p ".")
        (setq do-indent t)))

    (equal do-indent t)))

;;;###autoload
(defun jcs-nasm-return ()
  "Return key for `nasm-mode'."
  (interactive)

  (let ((continue-comment nil))
    (save-excursion
      (ignore-errors
        (jcs-goto-first-char-in-line)

        (forward-char 1)
        (when (current-char-equal-p ";")
          (forward-char 1)
          (when (current-char-equal-p ";")
            (setq continue-comment t)))))

    (newline-and-indent)

    (when (equal continue-comment t)
      (insert ";; ")
      (save-excursion
        (indent-line-to 0)))))

;;;###autoload
(defun jcs-nasm-comment ()
  "Comment key for `nasm-mode'."
  (interactive)

  ;; Call normal nasm comment function before do our
  ;; own nasm comment.
  (call-interactively 'nasm-comment)

  (let ((should-indent nil))
    (save-excursion
      (backward-char 1)
      (when (current-char-equal-p ";")
        (backward-char 1)
        (when (is-met-first-char-at-line-p)
          (setq should-indent t)
          ;; Indent it to the very left/beginning of line.
          (indent-line-to 0))))

    (save-excursion
      ;; If search backward failed, try forward.
      (when (equal should-indent nil)
        (forward-char 1)
        (when (current-char-equal-p ";")
          (setq should-indent t)
          ;; Indent it to the very left/beginning of line.
          (indent-line-to 0))))

    (when (and (equal should-indent t)
               (is-end-of-line-p))
      (insert " "))))
