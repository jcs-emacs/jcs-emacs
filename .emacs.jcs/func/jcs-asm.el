;;; jcs-asm.el --- Assembly Language related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; First load the mode to prevent overwrite after.
(require 'masm-mode)
(require 'nasm-mode)

(defvar jcs-asm--asking-mode nil
  "Flag for asking the Assembly Language mode.")

(defun jcs-asm-ask-mode (mode)
  "Ask the MODE to run."
  (interactive
   (list (completing-read
          "Major mode for this Assembly Language file: "
          '("masm" "nasm"))))
  (pcase mode
    ("masm" (masm-mode))
    ("nasm" (nasm-mode))))

(defun jcs-asm-ask-source (sc)
  "Ask the source SC for editing Assembly Language file."
  (interactive
   (list (completing-read
          "Major source for this Assembly Language file: "
          '("masm" "nasm"))))
  (let ((jcs-asm--asking-mode t))
    (pcase sc
      ("masm" (masm-mode) (jcs-insert-masm-template))
      ("nasm" (nasm-mode) (jcs-insert-nasm-template)))))


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
            ;; ATTENTION: Self copy this function from source code. Add these
            ;; rules for my own use.
            (jcs-is-nasm-indent-p)
            ;;--------------------------------------------------
            (looking-at nasm-label-regexp))
        (indent-line-to 0)
      (indent-line-to nasm-basic-offset))
    (when (> (- (point-max) orig) (point))
      (goto-char (- (point-max) orig)))))

(defun jcs-is-nasm-indent-p ()
  "JayCeS's own indent nasm rules.
@return boolean : true - do indent, false - vice versa."
  (let (do-indent)
    (save-excursion
      ;; Goto the first character of current line.
      (jcs-back-to-indentation-or-beginning)
      (when (jcs-is-beginning-of-line-p)
        (jcs-back-to-indentation-or-beginning))
      (forward-char 1)
      ;; Check rule here..
      (when (jcs-current-char-equal-p ".")
        (setq do-indent t)))
    do-indent))

(defun jcs-asm-return ()
  "Return key for `nasm-mode'."
  (interactive)
  (let (continue-comment)
    (save-excursion
      (ignore-errors
        (jcs-goto-first-char-in-line)

        (forward-char 1)
        (when (jcs-current-char-equal-p ";")
          (forward-char 1)
          (when (jcs-current-char-equal-p ";")
            (setq continue-comment t)))))

    (newline-and-indent)

    (when continue-comment
      (insert ";; ")
      (save-excursion (indent-line-to 0)))))

(defun jcs-asm-comment ()
  "Comment key for `nasm-mode'."
  (interactive)
  ;; Call normal nasm comment function before do our own nasm comment.
  (call-interactively 'nasm-comment)

  (let (should-indent)
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
      (unless should-indent
        (forward-char 1)
        (when (jcs-current-char-equal-p ";")
          (setq should-indent t)
          ;; Indent it to the very left/beginning of line.
          (indent-line-to 0))))

    (when (and should-indent (jcs-is-end-of-line-p))
      (insert " "))))

(provide 'jcs-asm)
;;; jcs-asm.el ends here
