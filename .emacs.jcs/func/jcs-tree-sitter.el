;;; jcs-tree-sitter.el --- TypeScript related  -*- lexical-binding: t -*-
;;; Commentary: When editing the TypeScript related file.
;;; Code:

(require 'tree-sitter-langs)

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(jcs-funcall-fboundp 'jcs-reset-common-faces-by-theme)

(defconst jcs-tree-sitter-queries-dir "~/.emacs.jcs/queries/"
  "Path points to your own tree-sitter query files.")

(defun jcs--tree-sitter-langs--hl-query-path (lang-symbol)
  "Advice override function `tree-sitter-langs--hl-query-path'."
  (setq lang-symbol (symbol-name lang-symbol))
  (let ((own (file-name-as-directory (concat jcs-tree-sitter-queries-dir lang-symbol)))
        (default (file-name-as-directory (concat tree-sitter-langs--queries-dir lang-symbol))))
    (if (file-directory-p own) own default)))
(advice-add 'tree-sitter-langs--hl-query-path :override #'jcs--tree-sitter-langs--hl-query-path)

(defun jcs--tree-sitter-langs--query-conversion (query)
  "Convert QUERY so it matches the core `emacs-tree-sitter' engine."
  (require 's)
  (when (stringp query)
    (setq query (s-replace "#match?" ".match?" query)
          query (s-replace "#eq?" ".eq?" query)
          query (s-replace "#is-not?" ".is-not?" query)))
  query)

(defun jcs--tree-sitter-langs--hl-default-patterns (fnc &rest args)
  "Advice override function `tree-sitter-langs--hl-default-patterns'."
  (jcs--tree-sitter-langs--query-conversion (apply fnc args)))
(advice-add 'tree-sitter-langs--hl-default-patterns :around #'jcs--tree-sitter-langs--hl-default-patterns)

;; TODO: Override this for now.
(defun tree-sitter-langs--hl-default-patterns (lang-symbol)
  "Return the bundled default syntax highlighting patterns for LANG-SYMBOL.
Return nil if there are no bundled patterns."
  (condition-case nil
      (with-temp-buffer
        ;; TODO: Make this less ad-hoc.
        (dolist (sym (cons lang-symbol
                           (pcase lang-symbol
                             ('cpp '(c))
                             ('typescript '(javascript))
                             (_ nil))))
          (let* ((path (tree-sitter-langs--hl-query-path sym))
                 (quries (directory-files path t "\\.scm$")))
            (dolist (query quries)
              (insert-file-contents query)
              (goto-char (point-max))
              (insert "\n"))))
        (buffer-string))
    (file-missing nil)))

;;
;; (@* "Faces" )
;;

(defface tree-sitter-hl-face:css.id
  '((t (:foreground "#D68974")))
  "Highlight CSS id."
  :group 'jcs)
(defvar tree-sitter-hl-face:css.class 'tree-sitter-hl-face:css.class)

(defface tree-sitter-hl-face:css.class
  '((t (:foreground "#FAD42D")))
  "Highlight CSS class."
  :group 'jcs)
(defvar tree-sitter-hl-face:css.class 'tree-sitter-hl-face:css.class)

(provide 'jcs-tree-sitter)
;;; jcs-tree-sitter.el ends here
