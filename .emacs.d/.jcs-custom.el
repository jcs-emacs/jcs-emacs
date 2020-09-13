(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(fill-page organize-imports-java ivy-file-preview eshell-syntax-highlighting powershell searcher license-templates ivy-searcher impatient-showdown better-scroll elisp-demos quelpa powerline fountain-mode htmlize atl-markup elisp-refs emojify async edit-indirect bind-key dash test-sha reload-emacs multi-shell jayces-mode file-header most-used-words lua-mode nix-mode auto-read-only groovy-mode gitignore-templates grammarly helpful keypression transwin mmm-mode ialign posframe treemacs avy github-browse-file markdown-mode transient dash-functional projectile popup with-editor hydra google-translate lsp-treemacs git-commit ivy flycheck csproj-mode lsp-mode yasnippet-snippets yasnippet yascroll yaml-mode which-key web-mode impatient-mode vue-mode vs-light-theme vs-dark-theme visual-regexp vimrc-mode use-ttf use-package undo-tree typescript-mode togetherly swift-mode sql-indent smex scss-mode ssass-mode show-eol shader-mode scala-mode rust-mode rjsx-mode right-click-context reveal-in-folder restart-emacs region-occurrences-highlighter rainbow-mode python-mode project-abbrev processing-mode preproc-font-lock parse-it package-lint package-build org-bullets nhexl-mode neotree nasm-mode multiple-cursors move-text masm-mode markdown-toc manage-minor-mode-table manage-minor-mode magit lsp-ui lsp-origami lsp-java line-reminder kotlin-mode json-mode js2-mode javadoc-lookup isearch-project ini-mode indicators iedit htmltagwrap hl-todo highlight-indent-guides haxe-mode haskell-mode goto-line-preview goto-char-preview google-this go-mode glsl-mode gitignore-mode gitconfig-mode gitattributes-mode gdscript-mode flycheck-pos-tip flycheck-popup-tip flycheck-grammarly flx ffmpeg-player feebleline exec-path-from-shell esup ess erlang emoji-github emmet-mode elixir-mode elisp-def dumb-jump dockerfile-mode diminish-buffer diminish define-it dashboard-ls dashboard dart-mode dap-mode csharp-mode counsel-projectile counsel company-quickhelp-terminal company-quickhelp company-fuzzy company-emoji company command-log-mode com-css-sort cobol-mode cmake-font-lock clojure-mode centaur-tabs browse-kill-ring buffer-wrap buffer-move basic-mode auto-rename-tag auto-highlight-symbol apache-mode alt-codes adaptive-wrap actionscript-mode ace-window))
 '(safe-local-variable-values
   '((lsp--override-calculate-lisp-indent\? . t)
     (flycheck-disabled-checkers quote
                                 (emacs-lisp-checkdoc))
     (eval progn
           (let
               ((dirloc-lsp-defun-regexp
                 (concat
                  (concat "^\\s-*(" "lsp-defun" "\\s-+\\(")
                  (or
                   (bound-and-true-p lisp-mode-symbol-regexp)
                   "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")
                  "\\)")))
             (add-to-list 'imenu-generic-expression
                          (list "Functions" dirloc-lsp-defun-regexp 1)))
           (defvar lsp--override-calculate-lisp-indent\? nil "Whether to override `lisp-indent-function' with
              the updated `calculate-lisp-indent' definition from
              Emacs 28.")
           (defun wrap-calculate-lisp-indent
               (func &optional parse-start)
             "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

PARSE-START may be a buffer position to start parsing from, or a
parse state as returned by calling `parse-partial-sexp' up to the
beginning of the current line.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression."
             (if
                 (not lsp--override-calculate-lisp-indent\?)
                 (funcall func parse-start)
               (save-excursion
                 (beginning-of-line)
                 (let
                     ((indent-point
                       (point))
                      state
                      (desired-indent nil)
                      (retry t)
                      whitespace-after-open-paren calculate-lisp-indent-last-sexp containing-sexp)
                   (cond
                    ((or
                      (markerp parse-start)
                      (integerp parse-start))
                     (goto-char parse-start))
                    ((null parse-start)
                     (beginning-of-defun))
                    (t
                     (setq state parse-start)))
                   (unless state
                     (while
                         (<
                          (point)
                          indent-point)
                       (setq state
                             (parse-partial-sexp
                              (point)
                              indent-point 0))))
                   (while
                       (and retry state
                            (>
                             (elt state 0)
                             0))
                     (setq retry nil)
                     (setq calculate-lisp-indent-last-sexp
                           (elt state 2))
                     (setq containing-sexp
                           (elt state 1))
                     (goto-char
                      (1+ containing-sexp))
                     (if
                         (and calculate-lisp-indent-last-sexp
                              (> calculate-lisp-indent-last-sexp
                                 (point)))
                         (let
                             ((peek
                               (parse-partial-sexp calculate-lisp-indent-last-sexp indent-point 0)))
                           (if
                               (setq retry
                                     (car
                                      (cdr peek)))
                               (setq state peek)))))
                   (if retry nil
                     (goto-char
                      (1+ containing-sexp))
                     (setq whitespace-after-open-paren
                           (looking-at
                            (rx whitespace)))
                     (if
                         (not calculate-lisp-indent-last-sexp)
                         (setq desired-indent
                               (current-column))
                       (parse-partial-sexp
                        (point)
                        calculate-lisp-indent-last-sexp 0 t)
                       (cond
                        ((looking-at "\\s("))
                        ((>
                          (save-excursion
                            (forward-line 1)
                            (point))
                          calculate-lisp-indent-last-sexp)
                         (if
                             (or
                              (=
                               (point)
                               calculate-lisp-indent-last-sexp)
                              whitespace-after-open-paren)
                             nil
                           (progn
                             (forward-sexp 1)
                             (parse-partial-sexp
                              (point)
                              calculate-lisp-indent-last-sexp 0 t)))
                         (backward-prefix-chars))
                        (t
                         (goto-char calculate-lisp-indent-last-sexp)
                         (beginning-of-line)
                         (parse-partial-sexp
                          (point)
                          calculate-lisp-indent-last-sexp 0 t)
                         (backward-prefix-chars)))))
                   (let
                       ((normal-indent
                         (current-column)))
                     (cond
                      ((elt state 3)
                       nil)
                      ((and
                        (integerp lisp-indent-offset)
                        containing-sexp)
                       (goto-char containing-sexp)
                       (+
                        (current-column)
                        lisp-indent-offset))
                      (calculate-lisp-indent-last-sexp
                       (or
                        (and lisp-indent-function
                             (not retry)
                             (funcall lisp-indent-function indent-point state))
                        (and
                         (save-excursion
                           (goto-char indent-point)
                           (skip-chars-forward " 	")
                           (looking-at ":"))
                         (save-excursion
                           (goto-char calculate-lisp-indent-last-sexp)
                           (backward-prefix-chars)
                           (while
                               (not
                                (or
                                 (looking-back "^[ 	]*\\|([ 	]+"
                                               (line-beginning-position))
                                 (and containing-sexp
                                      (>=
                                       (1+ containing-sexp)
                                       (point)))))
                             (forward-sexp -1)
                             (backward-prefix-chars))
                           (setq calculate-lisp-indent-last-sexp
                                 (point)))
                         (> calculate-lisp-indent-last-sexp
                            (save-excursion
                              (goto-char
                               (1+ containing-sexp))
                              (parse-partial-sexp
                               (point)
                               calculate-lisp-indent-last-sexp 0 t)
                              (point)))
                         (let
                             ((parse-sexp-ignore-comments t)
                              indent)
                           (goto-char calculate-lisp-indent-last-sexp)
                           (or
                            (and
                             (looking-at ":")
                             (setq indent
                                   (current-column)))
                            (and
                             (<
                              (line-beginning-position)
                              (prog2
                                  (backward-sexp)
                                  (point)))
                             (looking-at ":")
                             (setq indent
                                   (current-column))))
                           indent))
                        normal-indent))
                      (desired-indent)
                      (t normal-indent)))))))
           (when
               (< emacs-major-version 28)
             (advice-add #'calculate-lisp-indent :around #'wrap-calculate-lisp-indent))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "dark gray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-scrollbar-bg ((t (:background "dark gray"))))
 '(company-scrollbar-fg ((t (:background "black"))))
 '(company-tooltip ((t (:background "light gray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:background "light gray" :foreground "#C00000"))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:background "steel blue" :foreground "#C00000"))))
 '(company-tooltip-selection ((t (:background "steel blue" :foreground "white")))))

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)  ; Enable downcase-region
(put 'upcase-region 'disabled nil)    ; Enable upcase-region
