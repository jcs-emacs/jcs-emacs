;;; jcs-asm-mode.el --- Assembly Language related modes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
      (when (jcs-beginning-of-line-p)
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

    (when (and should-indent (jcs-end-of-line-p))
      (insert " "))))

(defun jcs-asm-mode--init ()
  "Do insert file header and switch major mode picked."
  (when (and (not jcs-asm--asking-mode)
             ;; Insert file header.
             (not (jcs-insert-header-if-valid '("[.]asm"
                                                "[.]inc")
                                              'jcs-asm-ask-source
                                              :interactive t)))
    ;; Switch major mode.
    (let ((jcs-asm--asking-mode t)) (call-interactively #'jcs-asm-ask-mode))))

;;
;; (@* "Templates" )
;;

(defun jcs-insert-masm-template ()
  "Header for MASM file."
  (jcs--file-header--insert "assembly" "masm.txt"))

(defun jcs-insert-nasm-template ()
  "Header for NASM file."
  (jcs--file-header--insert "assembly" "nasm.txt"))

;;
;; (@* "Hook" )
;;

(defun jcs-masm-mode-hook ()
  "MASM mode hook."
  (electric-pair-mode nil)
  (modify-syntax-entry ?_ "w")

  (jcs-asm-mode--init)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  ;; Comment
  (jcs-bind-key (kbd "RET") #'jcs-asm-return)
  (jcs-bind-key (kbd ";") #'jcs-asm-comment))

(add-hook 'masm-mode-hook 'jcs-masm-mode-hook)

(defun jcs-nasm-mode-hook ()
  "NASM mode hook."
  (electric-pair-mode nil)

  (modify-syntax-entry ?_ "w")

  (jcs-asm-mode--init)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  ;; Comment
  (jcs-bind-key (kbd "RET") #'jcs-asm-return)
  (jcs-bind-key (kbd ";") #'jcs-asm-comment))

(add-hook 'nasm-mode-hook 'jcs-nasm-mode-hook)

(provide 'jcs-asm-mode)
;;; jcs-asm-mode.el ends here
