;;; lang/python/config.el  -*- lexical-binding: t; -*-

(require 'python)
(require 'python-mode)

;;
;; (@* "Keys" )
;;

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

(file-header-defsrc jcs-ask-python-template "Select Python template: "
  '(("Plain" . "Start literate programming")
    ("Class" . "Start object-oriented programming (OOP)"))
  (pcase index
    (0 (jcs-insert-python-template))
    (1 (jcs-insert-python-class-template))))

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

(use-package python
  :init
  (setq python-indent-guess-indent-offset nil))

(jcs-add-hook 'python-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]py")
                              'jcs-ask-python-template
                              :interactive t)

  (jcs-key-local
    `(((kbd "RET") . jcs-py-return))))

;;
;; (@* "Extensions" )
;;

(use-package python-pytest
  :init
  (setq python-pytest-preferred-project-manager 'project
        ;; Don't rely on Tree-sitter.
        python-pytest-use-treesit nil))
