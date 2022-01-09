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
  (and (not (jcs-beginning-of-line-p))
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
  (jcs-contain-list-string jcs-py-keywords in-keyword))

;;
;; (@* "Header" )
;;

(defun jcs-ask-python-template (type)
  (interactive
   (list (completing-read
          "Type of the Python template: " '("Class" "Plain"))))
  (pcase type
    ("Class" (jcs-insert-python-class-template))
    ("Plain" (jcs-insert-python-template))))

;;
;; (@* "Templates" )
;;

(defun jcs-insert-python-template ()
  "Python template."
  (jcs--file-header--insert "python" "default.txt"))

(defun jcs-insert-python-class-template ()
  "Python class template."
  (jcs--file-header--insert "python" "class.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'python-mode-hook
  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]py")
                              'jcs-ask-python-template
                              :interactive t)

  (jcs-key-local
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next))
      ((kbd "<backspace>") . jcs-smart-backspace)
      ([C-backspace]       . jcs-backward-delete-word)
      ([M-up]   . jcs-previous-blank-line)
      ([M-down] . jcs-next-blank-line)
      ;; Edit
      ((kbd "<delete>") . jcs-smart-delete)
      ((kbd "TAB")      . jcs-tab-key)
      ((kbd "RET") . jcs-py-return)
      ((kbd "C-v") . yank))))

(provide 'jcs-python-mode)
;;; jcs-python-mode.el ends here
