;;; jcs-python-mode.el --- Python mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'python)
(require 'python-mode)

(setq python-indent-guess-indent-offset nil)

(defun jcs-py-return ()
  "Return key for `python-mode'."
  (interactive)
  (call-interactively #'newline)
  (py-indent-line-outmost))

(defun jcs-py-safe-backward-delete-char ()
  "Backward delete char safely in `python-mode'."
  (when (jcs-py-check-backward-delete-space)
    (backward-delete-char 1)))

(defun jcs-py-check-backward-delete-space ()
  "Check able to backward delete the space."
  (and (not (bolp))
       ;; Make sure is not a tab.
       (jcs-current-char-equal-p " ")))


(defun jcs-py-check-first-char-of-line-is-keyword-p ()
  "Check the first character of the current line the keyword line."
  (let (is-keyword)
    (save-excursion
      (back-to-indentation)
      (forward-char 1)
      (when (jcs-current-char-equal-p "@")
        (forward-char 1))
      (when (jcs-py-is-python-keyword (word-at-point))
        (setq is-keyword t)))
    is-keyword))


(defconst jcs-py-keywords
  '("class"
    "classmethod"
    "def"
    "from"
    "import"
    "staticmethod")
  "List of `python' keyword.")

(defun jcs-py-is-python-keyword (in-keyword)
  "Check if the current word is in the `python-keyword-list'."
  (member in-keyword jcs-py-keywords))

;;
;; (@* "Header" )
;;

(file-header-defsrc jcs-ask-python-template "Type of the Python template: "
  '("Class" "Plain")
  (pcase index
    (0 (jcs-insert-python-class-template))
    (1 (jcs-insert-python-template))))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-python-template "python" "default.txt"
  "Python template.")

(file-header-defins jcs-insert-python-class-template "python" "class.txt"
  "Python class template.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'python-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  ;; File Header
  (jcs-insert-header-if-valid '("[.]py")
                              'jcs-ask-python-template
                              :interactive t)

  (jcs-key-local
    `(((kbd "<up>")        . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>")      . ,(jcs-get-prev/next-key-type 'next))
      ((kbd "<backspace>") . jcs-smart-backspace)
      ([C-backspace]       . jcs-backward-delete-word)
      ([M-up]              . jcs-previous-blank-line)
      ([M-down]            . jcs-next-blank-line)
      ((kbd "<delete>")    . jcs-smart-delete)
      ((kbd "TAB")         . jcs-tab-key)
      ((kbd "RET")         . jcs-py-return)
      ((kbd "C-v")         . yank))))

(provide 'jcs-python-mode)
;;; jcs-python-mode.el ends here
