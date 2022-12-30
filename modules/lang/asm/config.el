;;; lang/asm/config.el  -*- lexical-binding: t; -*-

(require 'masm-mode)
(require 'nasm-mode)

;;
;; (@* "Keys" )
;;

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
      (back-to-indentation)
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
        (back-to-indentation)
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

(defun jcs-asm-ask-mode (mode)
  "Ask the MODE to run."
  (interactive
   (list (completing-read
          "Major mode for this Assembly Language file: "
          '("masm" "nasm"))))
  (pcase mode
    ("masm" (masm-mode))
    ("nasm" (nasm-mode))))

(file-header-defsrc jcs-asm-ask-source
    "Major source for this Assembly Language file: "
  '(("masm" . "Microsoft Macro Assembler")
    ("nasm" . "Netwide Assembler"))
  (let ((jcs-asm--asking-mode t))
    (pcase index
      (0 (masm-mode) (jcs-insert-masm-template))
      (1 (nasm-mode) (jcs-insert-nasm-template)))))

(file-header-defins jcs-insert-masm-template "assembly" "masm/default.txt"
  "Header for MASM file.")

(file-header-defins jcs-insert-nasm-template "assembly" "nasm/default.txt"
  "Header for NASM file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'masm-mode-hook
  (modify-syntax-entry ?_ "w")
  (jcs-asm-mode--init)
  (jcs-key-local
    `(((kbd "RET")    . jcs-asm-return)
      ((kbd ";")      . jcs-asm-comment))))

(jcs-add-hook 'nasm-mode-hook
  (modify-syntax-entry ?_ "w")
  (jcs-asm-mode--init)
  (jcs-key-local
    `(((kbd "RET")    . jcs-asm-return)
      ((kbd ";")      . jcs-asm-comment))))
