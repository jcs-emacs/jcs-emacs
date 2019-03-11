;;; jcs-nasm-func.el --- Assembly Language related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


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
      (jcs-back-to-indentation-or-beginning)
      (when (jcs-is-beginning-of-line-p)
        (jcs-back-to-indentation-or-beginning))
      (forward-char 1)

      ;; Check rule here..
      (when (jcs-current-char-equal-p ".")
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
        (when (jcs-current-char-equal-p ";")
          (forward-char 1)
          (when (jcs-current-char-equal-p ";")
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
      (when (jcs-current-char-equal-p ";")
        (backward-char 1)
        (when (jcs-is-infront-first-char-at-line-p)
          (setq should-indent t)
          ;; Indent it to the very left/beginning of line.
          (indent-line-to 0))))

    (save-excursion
      ;; If search backward failed, try forward.
      (when (equal should-indent nil)
        (forward-char 1)
        (when (jcs-current-char-equal-p ";")
          (setq should-indent t)
          ;; Indent it to the very left/beginning of line.
          (indent-line-to 0))))

    (when (and (equal should-indent t)
               (jcs-is-end-of-line-p))
      (insert " "))))


(provide 'jcs-nasm-func)
;;; jcs-nasm-func.el ends here
