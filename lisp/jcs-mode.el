;;; jcs-mode.el --- Self mode defines  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Mode State" )
;;

(defun jcs-reload-active-mode ()
  "Reload the active mode.
Note this is opposite logic to the toggle mode function."
  (interactive)
  (msgu-silent
    (cond
     ((jcs-backtrace-occurs-p) (jcs-hit-backtrace))
     ((active-minibuffer-window) (jcs-dark-blue-mode-line))
     ((ignore-errors (jcs-funcall-fboundp #'dap--cur-active-session-or-die))
      (jcs-dark-orange-mode-line))
     ((jcs-funcall-fboundp #'zoom-window--enable-p) (jcs-dark-green-mode-line))
     (t (jcs-gray-mode-line)))))

(defun jcs-buffer-spaces-to-tabs ()
  "Check if buffer using spaces or tabs."
  (if (= (how-many "^\t" (point-min) (point-max)) 0) "SPC" "TAB"))

(defun jcs-use-cc-style-comment ()
  "Use c-style commenting instead of two slashes."
  (setq-local comment-start "/*"
              comment-start-skip "/\\*+[ \t]*"
              comment-end "*/"
              comment-end-skip "[ \t]*\\*+/"))

(defun jcs-use-cc-mutliline-comment ()
  "Fixed multiline comment."
  (require 'typescript-mode)
  (setq-local indent-line-function 'typescript-indent-line)
  (setq c-comment-prefix-regexp "//+\\|\\**"
        c-paragraph-start "$"
        c-paragraph-separate "$"
        c-block-comment-prefix "* "
        c-line-comment-starter "//"
        c-comment-start-regexp "/[*/]\\|\\s!"
        comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (let ((c-buffer-is-cc-mode t))
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (make-local-variable 'adaptive-fill-mode)
    (make-local-variable 'adaptive-fill-regexp)
    (c-setup-paragraph-variables)))

;;
;; (@* "License" )
;;

;; Ask to insert the license content base on SOURCE.
(file-header-defsrc jcs-ask-insert-license-content "Type of the license: "
  (delete-dups
   (sort (append (list "Default (empty)") (license-templates-names))
         #'string-lessp))
  (cond ((string= source "Default (empty)") (progn ))
        ((member source (license-templates-names))
         (license-templates-insert source))))

;;
;; (@* "Change Log" )
;;

;; Ask to insert the changelog content base on SOURCE.
(file-header-defsrc jcs-ask-insert-changelog-content "Type of the changelog: "
  (append (list "Default (empty)")
          (jcs-dir-to-filename jcs-changelog-template-dir ".txt"))
  (pcase source
    ("Default (empty)" )  ; Do nothing...
    (_ (file-header-insert-template-by-file-path
        (format "%s%s.txt" jcs-changelog-template-dir source)))))

;;----------------------------------------------------------------------------
;;; Startup Modes

;; NOTE: These are modes that will startup immediately, meaning there will
;; be no benefits having in the separated files except the modulation.
;;
;; So just put all the startup modes' configuration here.

;;; Special
(jcs-add-hook 'special-mode-hook (goto-address-mode 1))

;;; Backtrace
(jcs-add-hook 'backtrace-mode-hook
  (buffer-wrap-mode 1)
  (jcs-key-local
    `(((kbd "M-k") . kill-buffer-and-window))))

;;; Buffer Menu
(jcs-add-hook 'Buffer-menu-mode-hook
  (require 'buffer-menu-project)

  (buffer-menu-filter-mode 1)
  (diminish-buffer-mode 1)  ; refresh the menu immediately

  (jcs-key-local
    `(((kbd "C-k"))
      ((kbd "M-K")      . buffer-menu-filter-refresh)
      ;; Searching / Filtering
      ((kbd "<escape>") . (lambda () (interactive) (buffer-menu-filter-refresh)
                            (top-level))))))

;;; Diff
(jcs-add-hook 'diff-mode-hook
  (jcs-key-local
    `(((kbd "M-k") . jcs-maybe-kill-this-buffer)
      ((kbd "M-K") . jcs-reopen-this-buffer))))

;;; Compilation
(jcs-add-hook '(compilation-mode-hook comint-mode-hook)
  (setq truncate-lines nil)

  (buffer-disable-undo)
  (goto-address-mode 1)

  ;; NOTE: Set smaller font.
  (setq buffer-face-mode-face '(:height 120))
  (buffer-face-mode)

  (jcs-key-local
    `(((kbd "C-S-<f11>") . compilation-previous-error)
      ((kbd "C-S-<f12>") . compilation-next-error))))

(jcs-add-hook 'emacs-lisp-compilation-mode-hook
  (setq truncate-lines t))

;;; Message Buffer
(jcs-add-hook 'messages-buffer-mode-hook
  (goto-address-mode 1)
  (page-break-lines-mode 1))

;;; Re-Builder
(jcs-add-hook 'reb-mode-hook
  (jcs-key-local
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line)
      ((kbd "M-k")    . kill-buffer-and-window))))

;;; Tabulated List
(jcs-add-hook 'tabulated-list-mode-hook
  (when (memq major-mode '(Buffer-menu-mode package-menu-mode))
    (buffer-wrap-mode 1)))

;;
;;; Project

(defun jcs-on-project-hook ()
  "Hook runs when there is valid project root."
  (when (jcs-funcall-fboundp #'jcs-project-under-p)
    (global-diff-hl-mode 1)
    (editorconfig-mode 1)
    (global-prettier-mode 1)
    (jcs--safe-lsp-active)))

;;
;;; Base Mode

(jcs-add-hook '(conf-mode-hook text-mode-hook prog-mode-hook)
  (alt-codes-mode 1)
  (auto-highlight-symbol-mode t)
  (display-fill-column-indicator-mode 1)
  (display-line-numbers-mode 1)
  (highlight-numbers-mode 1)
  (indent-control-ensure-tab-width)  ; Ensure indentation level is available
  (goto-address-mode 1)
  (when elenv-graphic-p (highlight-indent-guides-mode 1))
  (yas-minor-mode 1)

  (jcs-on-project-hook))

(jcs-add-hook 'text-mode-hook
  (company-fuzzy-backend-add 'company-kaomoji)

  (jcs-insert-header-if-valid
   '("\\(/\\|\\`\\)[Ll][Ii][Cc][Ee][Nn][Ss][Ee]") 'jcs-ask-insert-license-content
   :interactive t)
  (jcs-insert-header-if-valid
   '("\\(/\\|\\`\\)[Cc][Hh][Aa][Nn][Gg][Ee][-_]*[Ll][Oo][Gg]")
   'jcs-ask-insert-changelog-content
   :interactive t))

(jcs-add-hook 'prog-mode-hook
  ;; XXX: See the bug https://github.com/immerrr/lua-mode/issues/172
  (unless (jcs-contain-list-type-str "-" (list comment-start comment-end) 'regex)
    (modify-syntax-entry ?- "_")))

;;; Emacs Lisp
(jcs-add-hook 'emacs-lisp-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word.
  (jcs-insert-header-if-valid '("[.]el")
                              'jcs-insert-emacs-lisp-template)
  (eask-api-setup))

;;; Lisp
(jcs-add-hook 'lisp-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word.
  (jcs-insert-header-if-valid '("[.]lisp")
                              'jcs-insert-lisp-template))

;;; Lisp Interaction
(jcs-add-hook 'lisp-interaction-mode-hook
  (jcs-key-local
    `(((kbd "M-k") . jcs-scratch-buffer-maybe-kill)
      ((kbd "M-K") . jcs-scratch-buffer-refresh))))

;;----------------------------------------------------------------------------
;;; Modes

(defconst jcs-mode-load-alist
  '(
;;; Others
    (message          . jcs-message-mode)
    ((shell esh-mode) . jcs-shell-mode)
    (yasnippet        . jcs-snippet-mode)
;;; Languages
    (actionscript-mode)
    (ada-mode)
    (agda-mode)
    (applescript-mode)
    (arduino-mode)
    ((masm-mode nasm-mode) . jcs-asm-mode)
    (autoconf-mode)
    (basic-mode)
    (bat-mode              . jcs-batch-mode)
    (caml                  . jcs-caml-mode)
    (cc-mode               . (jcs-cc-mode
                              jcs-c-mode
                              jcs-c++-mode
                              jcs-java-mode
                              jcs-objc-mode))
    (clojure-mode)
    (cmake-mode)
    (cobol-mode)
    (coffee-mode)
    (conf-mode)
    (crystal-mode)
    (csharp-mode)
    (css-mode)
    (d-mode)
    (dart-mode)
    (dockerfile-mode)
    (elixir-mode)
    (elm-mode)
    (erlang                  . jcs-erlang-mode)
    (ess-r-mode              . jcs-r-mode)
    (fountain-mode)
    (fsharp-mode)
    (gdscript-mode)
    ((gitattributes-mode gitconfig-mode gitignore-mode)
     . jcs-git-mode)
    ((shader-mode glsl-mode hlsl-mode) . jcs-shader-mode)
    (go-mode)
    (groovy-mode)
    (haml-mode)
    (haskell-mode)
    (haxe-mode)
    (idris-mode)
    (ini-mode)
    (jayces-mode)
    (jenkinsfile-mode)
    (js                      . jcs-js-mode)
    (json-mode)
    (julia-mode)
    (kotlin-mode)
    (less-css-mode)
    (lua-mode)
    (make-mode               . jcs-makefile-mode)
    (markdown-mode)
    ((masm-mode nasm-mode)   . jcs-asm-mode)
    (mint-mode)
    (nginx-mode)
    (nim-mode)
    (nix-mode)
    (nxml-mode               . jcs-xml-mode)
    (opascal                 . jcs-opascal-mode)
    (org                     . jcs-org-mode)
    (pascal                  . jcs-pascal-mode)
    (perl-mode)
    (powershell              . jcs-powershell-mode)
    (processing-mode)
    (python-mode)
    (qml-mode)
    (racket-mode)
    (rjsx-mode               . jcs-jsx-mode)
    (ruby-mode)
    (rust-mode)
    (ssass-mode              . jcs-sass-mode)
    (scala-mode)
    (scss-mode)
    ((sh-script fish-mode)   . jcs-sh-mode)
    (sql                     . jcs-sql-mode)
    (swift-mode)
    (terraform-mode)
    (typescript-mode)
    (verilog-mode)
    (vhdl-mode)
    (vimrc-mode              . jcs-vimscript-mode)
    (vue-mode)
    ((web-mode sgml-mode)    . jcs-web-mode)
    (yaml-mode)
    (zig-mode))
  "Alist of config modules to load.")

(defun jcs-mode-load-requires ()
  "Evaluate through `jcs-mode-load-alist' for all required modules."
  (dolist (data jcs-mode-load-alist)
    (let* ((mode (car data)) (modules (or (cdr data) (intern (format "jcs-%s" mode)))))
      (jcs-with-eval-after-load mode (jcs-require modules)))))

;;;
;; Auto mode Management

(setq
 auto-mode-alist
 (append
  '(
;;; A
    ("\\.agda'?\\'"         . agda-mode)
;;; B
    ("\\.beancount'?\\'"    . beancount-mode)
;;; C
    ("\\.hin'?\\'"          . c++-mode)
    ("\\.cin'?\\'"          . c++-mode)
    ("\\.cpp'?\\'"          . c++-mode)
    ("\\.hpp'?\\'"          . c++-mode)
    ("\\.inl'?\\'"          . c++-mode)
    ("\\.rdc'?\\'"          . c++-mode)
    ("\\.cc'?\\'"           . c++-mode)
    ("\\.c8'?\\'"           . c++-mode)
    ("\\.h'?\\'"            . c++-mode)
    ("\\.c'?\\'"            . c++-mode)
    ("\\.ml[iylp]?$"        . caml-mode)
    ("\\.cob\\'"            . cobol-mode)
    ("\\.cbl'?\\'"          . cobol-mode)
    ("\\.cpy\\'"            . cobol-mode)
;;; E
    ("\\.el'?\\'"           . emacs-lisp-mode)
;;; G
    ("\\.dockerignore'?\\'" . gitignore-mode)
    ("\\.npmignore'?\\'"    . gitignore-mode)
    ("\\.unityignore'?\\'"  . gitignore-mode)
    ("\\.vscodeignore'?\\'" . gitignore-mode)
;;; J
    ("\\.js'?\\'"           . js-mode)
    ("\\.json'?\\'"         . json-mode)
;;; K
    ("\\.ktm'?\\'"          . kotlin-mode)
    ("\\.kts'?\\'"          . kotlin-mode)
;;; L
    ("\\.lisp'?\\'"         . lisp-mode)
;;; M
    ("\\.asm'?\\'"          . masm-mode)
    ("\\.inc'?\\'"          . masm-mode)
;;; N
    ("\\.asm'?\\'"          . nasm-mode)
    ("\\.inc'?\\'"          . nasm-mode)
;;; O
    ("\\.dpk'?\\'"          . opascal-mode)
    ("\\.dpr'?\\'"          . opascal-mode)
;;; S
    ("\\.sass'?\\'"         . ssass-mode)
    ("\\.shader'?\\'"       . shader-mode)
    ("\\.sln'?\\'"          . sln-mode)
;;; V
    ("\\.vue'?\\'"          . web-mode)
;;; W
    ("\\.phtml\\'"          . web-mode)
    ("\\.tpl\\.php\\'"      . web-mode)
    ("\\.erb\\'"            . web-mode)
    ("\\.mustache\\'"       . web-mode)
    ("\\.djhtml\\'"         . web-mode)
    ("\\.html?\\'"          . web-mode)
    ("\\.php?\\'"           . web-mode)
    ("\\.[agj]sp\\'"        . web-mode)
    ;;
    ("\\.as[cp]x\\'"        . web-mode)
    ("\\.cshtml\\'"         . web-mode)
    ("\\.[Mm]aster\\'"      . web-mode))
  auto-mode-alist))

(provide 'jcs-mode)
;;; jcs-mode.el ends here
