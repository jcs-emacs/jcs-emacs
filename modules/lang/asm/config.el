;;; lang/asm/config.el  -*- lexical-binding: t; -*-

(require 'asm-mode)

(require 'fasm-mode)
(require 'masm-mode)
(require 'nasm-mode)
(require 'gas-mode)

;;
;; (@* "Keys" )
;;

(defun jcs-asm-comment ()
  "Comment key for `nasm-mode'."
  (interactive)
  ;; Call normal nasm comment function before do our own nasm comment.
  (call-interactively #'nasm-comment)

  (let (should-indent)
    (save-excursion
      (backward-char 1)
      (when (jcs-current-char-equal-p ";")
        (backward-char 1)
        (when (jcs-infront-first-char-at-line-p)
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

    (when (and should-indent (eolp))
      (insert " "))))

;;
;; (@* "Mode Detection" )
;;

(defvar jcs-asm--asking-mode nil
  "Flag for asking the Assembly Language mode.")

(defun jcs-asm-mode--init ()
  "Do insert file header and switch major mode picked."
  (when (and (not jcs-asm--asking-mode)
             (not (alist-get 'mode (hack-local-variables-prop-line)))
             ;; Insert file header.
             (not (jcs-insert-header-if-valid '("[.]asm"
                                                "[.]inc"
                                                "[.]s")
                                              'jcs-asm-ask-source
                                              :interactive t)))
    ;; Switch major mode.
    (let ((jcs-asm--asking-mode t)) (call-interactively #'jcs-asm-ask-mode))))

;;
;; (@* "Templates" )
;;

(defun jcs-asm-ask-mode (mode)
  "Ask the MODE to run."
  (interactive
   (list (completing-read
          "Major mode for this Assembly Language file: "
          '("asm" "fasm" "masm" "nasm" "gas"))))
  (pcase mode
    ("asm"  (asm-mode))
    ("fasm" (fasm-mode))
    ("masm" (masm-mode))
    ("nasm" (nasm-mode))
    ("gas"  (gas-mode))))

(file-header-defsrc jcs-asm-ask-source
    "Major source for this Assembly Language file: "
  '(("asm"  . "Default")
    ("fasm" . "Flat Assembler")
    ("masm" . "Microsoft Macro Assembler")
    ("nasm" . "Netwide Assembler")
    ("gas"  . "GNU Assembler"))
  (let ((jcs-asm--asking-mode t))
    (pcase index
      (0 (asm-mode)  (jcs-insert-asm-template))
      (1 (fasm-mode) (jcs-insert-fasm-template))
      (2 (masm-mode) (jcs-insert-masm-template))
      (3 (nasm-mode) (jcs-insert-nasm-template))
      (4 (gas-mode)  (jcs-insert-gas-template)))))

(file-header-defins jcs-insert-asm-template "assembly" "asm/default.txt"
  "Header for ASM file.")

(file-header-defins jcs-insert-fasm-template "assembly" "fasm/default.txt"
  "Header for FASM file.")

(file-header-defins jcs-insert-masm-template "assembly" "masm/default.txt"
  "Header for MASM file.")

(file-header-defins jcs-insert-nasm-template "assembly" "nasm/default.txt"
  "Header for NASM file.")

(file-header-defins jcs-insert-gas-template "assembly" "gas/default.txt"
  "Header for GAS file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook '( asm-mode-hook
                 fasm-mode-hook masm-mode-hook nasm-mode-hook)
  (jcs-asm-mode--init)
  (jcs-key-local
    `(((kbd ";") . jcs-asm-comment))))

(jcs-add-hook '( gas-mode-hook)
  (run-hooks 'prog-mode-hook)
  (jcs-asm-mode--init))

;;
;; (@* "Extensions" )
;;

(use-package flymake-nasm
  :hook (nasm-mode . flymake-nasm-setup))
