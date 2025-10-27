;;; lang/emacs-lisp/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-emacs-lisp-dir-locals-template "elisp" "dir-locals.txt"
  "Template for Emacs Lisp for .dir-locals file.")

(file-header-defins jcs-insert-emacs-lisp-template "elisp" "default.txt"
  "Template for Emacs Lisp.")

(file-header-defins jcs-insert-lisp-template "lisp" "default.txt"
  "Lisp file header format.")

;;
;; (@* "ElDoc" )
;;

(defun jcs--eldoc-remove-signature (str)
  "Remove function signature from STR."
  (with-temp-buffer
    (insert str)
    (goto-char (point-max))
    (when (jcs-current-char-equal-p ")")
      (backward-sexp)
      (delete-region (point) (point-max)))
    (string-trim (buffer-string))))

(defun jcs--elisp-eldoc-var-docstring-with-value (callback &rest _)
  "Edited from the function `elisp-eldoc-var-docstring-with-value'."
  (when-let* ((cs (elisp--current-symbol)))
    (when (and (boundp cs)
               ;; nil and t are boundp!
               (not (null cs))
               (not (eq cs t)))
      (funcall callback
               (concat (let* ((sym-val (symbol-value cs))
                              (sym-val (format "%.100S" sym-val))
                              (sym-val (s-chop-suffix "\n" sym-val)))
                         (propertize sym-val
                                     'face 'font-lock-preprocessor-face))
                       (let* ((types (elenv-types cs))
                              (types (mapcar (lambda (type)
                                               (format ":%s" type))
                                             types))
                              (types-str (mapconcat #'elenv-2str types " ")))
                         (concat "\n\n"
                                 (propertize types-str
                                             'face 'font-lock-builtin-face)))
                       (let* ((doc (documentation-property
                                    cs 'variable-documentation t))
                              (more (- (length doc) 1000)))
                         (concat "\n\n"
                                 (propertize
                                  (jcs-fill-string
                                   (if (string= doc "nil")
                                       "Undocumented."
                                     doc))
                                  'face 'font-lock-doc-face)
                                 (when (> more 0)
                                   (format "[%sc more]" more)))))
               :thing cs
               :face 'font-lock-variable-name-face))))

(defun jcs--elisp-eldoc-funcall (callback &rest _ignored)
  "Edited from the function `elisp-eldoc-funcall'."
  (let* ((sym-info (elisp--fnsym-in-current-sexp))
         (fn-sym (car sym-info))
         (doc (or (ignore-errors (documentation fn-sym t))
                  ""))
         (doc (jcs--eldoc-remove-signature doc))
         (doc (jcs-fill-string doc)))
    (when fn-sym
      (funcall callback
               (if-let* ((sig (apply #'elisp-get-fnsym-args-string sym-info)))
                   (format "%s\n\n%s" sig (propertize doc 'face 'font-lock-doc-face))
                 sig)
               :thing fn-sym
               :face (if (functionp fn-sym)
                         'font-lock-function-name-face
                       'font-lock-keyword-face)))))

;;
;; (@* "Hooks" )
;;

(jcs-add-hook 'emacs-lisp-mode-hook
  (jcs-insert-header-if-valid '("[.]el")
                              (if (equal (buffer-name) dir-locals-file)
                                  'jcs-insert-emacs-lisp-dir-locals-template
                                'jcs-insert-emacs-lisp-template))

  (company-fuzzy-backend-add-before 'company-elisp-keywords 'company-dabbrev)

  (add-hook 'eldoc-documentation-functions
            #'jcs--elisp-eldoc-funcall nil t)
  (add-hook 'eldoc-documentation-functions
            #'jcs--elisp-eldoc-var-docstring-with-value nil t)

  (eask-api-setup))

(jcs-add-hook 'emacs-lisp-compilation-mode-hook
  (setq truncate-lines t))

(jcs-add-hook 'lisp-mode-hook
  (jcs-insert-header-if-valid '("[.]lisp")
                              'jcs-insert-lisp-template))

(jcs-add-hook 'lisp-interaction-mode-hook
  (jcs-key-local
    `(((kbd "M-K") . jcs-scratch-buffer-refresh))))

(jcs-add-hook 'eask-mode-hook
  (company-fuzzy-backend-add-before 'company-eask 'company-dabbrev)
  (eldoc-eask-enable))

;;
;; (@* "Extensions" )
;;

(use-package elisp-demos
  :init
  (jcs-with-eval-after-load '(jcs-poptip) (org-load-modules-maybe))
  (jcs-advice-add 'org-load-modules-maybe :after
    (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package flycheck-cask    :hook (flycheck-mode . flycheck-cask-setup))
(use-package flycheck-eask    :hook (flycheck-mode . flycheck-eask-setup))

(use-package flycheck-elsa
  :hook (flycheck-mode . flycheck-elsa-setup)
  :init
  (setq flycheck-elsa-backend 'eask))

(use-package flycheck-package :hook (flycheck-mode . flycheck-package-setup))
(use-package flycheck-relint  :hook (flycheck-mode . flycheck-relint-setup))

(use-package sideline-eros
  :hook (sideline-mode . sideline-eros-setup))
