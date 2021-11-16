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
  (and (not (jcs-is-beginning-of-line-p))
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
      (when (jcs-py-is-python-keyword (jcs-get-word-at-point))
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
;; (@* "Templates" )
;;

(defun jcs-ask-python-template (type)
  (interactive
   (list (completing-read
          "Type of the Python template: " '("Class" "Plain"))))
  (pcase type
    ("Class" (jcs-insert-python-class-template))
    ("Plain" (jcs-insert-python-template))))

;;
;; (@* "Hook" )
;;

(defun jcs-python-mode-hook ()
  "Python mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]py")
                              'jcs-ask-python-template
                              :interactive t)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  (jcs-bind-key (kbd "<backspace>") #'jcs-smart-backspace)
  (jcs-bind-key [C-backspace] #'jcs-backward-delete-word)

  (jcs-bind-key [M-up] #'jcs-previous-blank-line)
  (jcs-bind-key [M-down] #'jcs-next-blank-line)

  ;; Edit
  (jcs-bind-key (kbd "<delete>") #'jcs-smart-delete)
  (jcs-bind-key (kbd "TAB") #'jcs-tab-key)

  (jcs-bind-key (kbd "RET") #'jcs-py-return)
  (jcs-bind-key (kbd "C-v") #'yank))

(add-hook 'python-mode-hook 'jcs-python-mode-hook)

(provide 'jcs-python-mode)
;;; jcs-python-mode.el ends here
