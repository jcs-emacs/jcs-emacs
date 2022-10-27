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
     ((jcs-funcall-fboundp #'jcs-backtrace-occurs-p) (jcs-hit-backtrace))
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

;;; Diff
(jcs-add-hook 'diff-mode-hook
  (jcs-key-local
    `(((kbd "M-k") . jcs-maybe-kill-this-buffer)
      ((kbd "M-K") . jcs-reopen-this-buffer))))

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
;;; Base Mode

(defvar jcs-on-project-hook nil
  "Hook run when the project is defined.")

(jcs-add-hook '(text-mode-hook prog-mode-hook conf-mode-hook)
  (alt-codes-mode 1)
  (auto-highlight-symbol-mode t)
  (display-fill-column-indicator-mode 1)
  (display-line-numbers-mode 1)
  (highlight-numbers-mode 1)
  (indent-control-ensure-tab-width)  ; Ensure indentation level is available
  (goto-address-mode 1)
  (when elenv-graphic-p (highlight-indent-guides-mode 1))
  (yas-minor-mode 1)

  (when (jcs-funcall-fboundp #'jcs-project-under-p)
    (run-hooks 'jcs-on-project-hook)))

(jcs-add-hook 'text-mode-hook
  (setq-local electric-pair-open-newline-between-pairs nil)

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

(jcs-add-hook 'conf-mode-hook
  (setq-local electric-pair-open-newline-between-pairs nil))

;;----------------------------------------------------------------------------
;;; Modes

(defconst jcs-module-load-alist
  '(
    (keypression             . "app/keypression")
    (elfeed                  . "app/rss")
    (company                 . "completion/company")
    (vertico                 . "completion/vertico")
    (ts-fold                 . "editor/fold")
    (multiple-cursors        . "editor/multiple-cursors")
    (backtrace               . "emacs/backtrace")
    (compile                 . "emacs/compile")
    (dired                   . "emacs/dired")
    (undo-tree               . "emacs/undo")
    ((gitattributes-mode gitconfig-mode gitignore-mode) . "emacs/vc")
    (dockerfile-mode         . "tools/dockerfile")
    (editorconfig            . "tools/editorconfig")
    (lsp-mode                . "tools/lsp")
    (make-mode               . "tools/make")
    (prettier                . "tools/prettier")
    (terraform-mode          . "tools/terraform")
    (tree-sitter             . "tools/tree-sitter")
    (dashboard               . "ui/dashboard")
    (emojify                 . "ui/emoji")
    (hl-todo                 . "ui/hl-todo")
    (highlight-indent-guides . "ui/indent-guides")
    (minimap                 . "ui/minimap")
    ((popup pos-tip)         . "ui/popup")
    (diff-hl                 . "ui/vc-gutter")
    (centaur-tabs            . "ui/tabs")
    (treemacs                . "ui/treemacs")
;;; Others
    (message                 . "email/message")
    (shell                   . "term/shell")
    (esh-mode                . "term/eshell")
    (yasnippet               . "editor/snippets")
;;; Languages
    (actionscript-mode       . "lang/actionscript")
    (ada-mode                . "lang/ada")
    (agda-mode               . "lang/agda")
    (applescript-mode        . "lang/applescript-mode")
    (arduino-mode            . "lang/arduino")
    ((masm-mode nasm-mode)   . "lang/asm")
    (autoconf-mode           . "lang/autoconf")
    (basic-mode              . "lang/basic")
    (bat-mode                . "lang/batch")
    (caml                    . "lang/caml")
    (cc-mode                 . ("lang/cc" "lang/c" "lang/c++"
                                "lang/java"
                                "lang/objc"))
    (clojure-mode            . "lang/clojure")
    (cmake-mode              . "lang/cmake")
    (cobol-mode              . "lang/cobol")
    (coffee-mode             . "lang/coffee")
    (conf-mode               . "lang/conf")
    (crystal-mode            . "lang/crystal")
    (csharp-mode             . "lang/csharp")
    (css-mode                . "lang/css")
    (d-mode                  . "lang/d")
    (dart-mode               . "lang/dart")
    (elixir-mode             . "lang/elixir")
    (elm-mode                . "lang/elm")
    (elisp-mode              . "lang/emacs-lisp")
    (erlang                  . "lang/erlang")
    (ess-r-mode              . "lang/r")
    (fountain-mode           . "lang/fountain")
    (fsharp-mode             . "lang/fsharp")
    (gdscript-mode           . "lang/gdscript")
    ((shader-mode glsl-mode hlsl-mode) . "lang/shader")
    (go-mode                 . "lang/go")
    (groovy-mode             . "lang/groovy")
    (haml-mode               . "lang/haml")
    (haskell-mode            . "lang/haskell")
    (haxe-mode               . "lang/haxe")
    (idris-mode              . "lang/idris")
    (ini-mode                . "lang/ini")
    (jayces-mode             . "lang/jayces")
    (jenkinsfile-mode        . "lang/jenkinsfile")
    (js                      . ("lang/js" "lang/jsx"))
    (json-mode               . "lang/json")
    (julia-mode              . "lang/julia")
    (kotlin-mode             . "lang/kotlin")
    (less-css-mode           . "lang/less-css")
    (lua-mode                . "lang/lua")
    (markdown-mode           . "lang/markdown")
    (mint-mode               . "lang/mint")
    (nginx-mode              . "lang/nginx")
    (nim-mode                . "lang/nim")
    (nix-mode                . "lang/nix")
    (nxml-mode               . "lang/xml")
    (opascal                 . "lang/opascal")
    (org                     . "lang/org")
    (pascal                  . "lang/pascal")
    (perl-mode               . "lang/perl")
    (powershell              . "lang/powershell")
    (processing-mode         . "lang/processing")
    (python-mode             . "lang/python")
    (qml-mode                . "lang/qml")
    (racket-mode             . "lang/racket")
    (ruby-mode               . "lang/ruby")
    (rust-mode               . "lang/rust")
    (ssass-mode              . "lang/sass")
    (scala-mode              . "lang/scala")
    (scss-mode               . "lang/scss")
    ((sh-script fish-mode)   . "lang/sh")
    (sql                     . "lang/sql")
    (swift-mode              . "lang/swift")
    (typescript-mode         . "lang/typescript")
    (verilog-mode            . "lang/verilog")
    (vhdl-mode               . "lang/vhdl")
    (vimrc-mode              . "lang/vimscript")
    (vue-mode                . "lang/vue")
    ((web-mode sgml-mode)    . "lang/web")
    (yaml-mode               . "lang/yaml")
    (zig-mode                . "lang/zig"))
  "Alist of config modules to load.")

(defun jcs-mode-load-requires ()
  "Evaluate through `jcs-module-load-alist' for all required modules."
  (dolist (data jcs-module-load-alist)
    (let ((mode (car data)) (modules (cdr data)))
      (jcs-with-eval-after-load mode (jcs-module-load modules)))))

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
