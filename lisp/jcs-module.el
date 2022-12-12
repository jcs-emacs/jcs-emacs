;;; jcs-module.el --- module & package management system  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;
;; Modules

(defconst jcs-module-load-alist
  '(
    (keypression                  . "app/keypression")
    (elfeed                       . "app/rss")
    (flycheck                     . "checkers/syntax")
    (company                      . "completion/company")
    (vertico                      . "completion/vertico")
    (tree-sitter                  . "editor/docstring")
    (expand-region                . "editor/expand-region")
    (file-header                  . "editor/file-templates")
    (ts-fold                      . "editor/fold")
    (isearch                      . "editor/isearch")
    ((iedit multiple-cursors)     . "editor/multiple-cursors")
    (yasnippet                    . "editor/snippets")
    ((vs-edit-mode vsc-edit-mode) . "editor/vs")
    (backtrace                    . "emacs/backtrace")
    (compile                      . "emacs/compile")
    (conf-mode                    . "emacs/conf-mode")
    (dired                        . "emacs/dired")
    (list-environment             . "emacs/list-thing")
    (re-builder                   . "emacs/re-builder")
    (text-mode                    . "emacs/text-mode")
    (undo-tree                    . "emacs/undo")
    (( gitattributes-mode gitconfig-mode gitignore-mode
       vc-refresh)
     . "emacs/vc")
    (message                      . "email/message")
    (flx                          . "misc/flx")
    (google-translate             . "misc/translator")
    (nov                          . "reader/epub")
    (esh-mode                     . "term/eshell")
    (shell                        . "term/shell")
    (dockerfile-mode              . "tools/dockerfile")
    (editorconfig                 . "tools/editorconfig")
    ((execrun quickrun)           . "tools/eval")
    ((goto-char-preview goto-line-preview) . "tools/goto")
    (lsp-mode                     . "tools/lsp")
    (make-mode                    . "tools/make")
    (magit                        . "tools/magit")
    (prettier                     . "tools/prettier")
    (terraform-mode               . "tools/terraform")
    (tree-sitter                  . "tools/tree-sitter")
    (dashboard                    . "ui/dashboard")
    (emojify                      . "ui/emoji")
    (hl-todo                      . "ui/hl-todo")
    (highlight-indent-guides      . "ui/indent-guides")
    (minimap                      . "ui/minimap")
    ((popup pos-tip)              . "ui/popup")
    (diff-hl                      . "ui/vc-gutter")
    (centaur-tabs                 . "ui/tabs")
    (treemacs                     . "ui/treemacs")
;;; Languages
    (actionscript-mode            . "lang/actionscript")
    (ada-mode                     . "lang/ada")
    (agda-mode                    . "lang/agda")
    (applescript-mode             . "lang/applescript-mode")
    (arduino-mode                 . "lang/arduino")
    ((masm-mode nasm-mode)        . "lang/asm")
    (autoconf-mode                . "lang/autoconf")
    (basic-mode                   . "lang/basic")
    (bat-mode                     . "lang/batch")
    (caml                         . "lang/caml")
    (cc-mode                      . ("lang/cc" "lang/c" "lang/c++"
                                     "lang/java"
                                     "lang/objc"))
    (clojure-mode                 . "lang/clojure")
    (cmake-mode                   . "lang/cmake")
    (cobol-mode                   . "lang/cobol")
    (coffee-mode                  . "lang/coffee")
    (conf-mode                    . "lang/conf")
    (crystal-mode                 . "lang/crystal")
    (csharp-mode                  . "lang/csharp")
    (css-mode                     . "lang/css")
    (d-mode                       . "lang/d")
    (dart-mode                    . "lang/dart")
    (elixir-mode                  . "lang/elixir")
    (elm-mode                     . "lang/elm")
    (elisp-mode                   . "lang/emacs-lisp")
    (erlang                       . "lang/erlang")
    (ess-r-mode                   . "lang/r")
    (fountain-mode                . "lang/fountain")
    (fsharp-mode                  . "lang/fsharp")
    (gdscript-mode                . "lang/gdscript")
    ((shader-mode glsl-mode hlsl-mode) . "lang/shader")
    (go-mode                      . "lang/go")
    (groovy-mode                  . "lang/groovy")
    (haml-mode                    . "lang/haml")
    (haskell-mode                 . "lang/haskell")
    (haxe-mode                    . "lang/haxe")
    (idris-mode                   . "lang/idris")
    (ini-mode                     . "lang/ini")
    (jayces-mode                  . "lang/jayces")
    (jenkinsfile-mode             . "lang/jenkinsfile")
    (js                           . ("lang/js" "lang/jsx"))
    (json-mode                    . "lang/json")
    (julia-mode                   . "lang/julia")
    (kotlin-mode                  . "lang/kotlin")
    (less-css-mode                . "lang/less-css")
    (lua-mode                     . "lang/lua")
    (markdown-mode                . "lang/markdown")
    (mint-mode                    . "lang/mint")
    (nginx-mode                   . "lang/nginx")
    (nim-mode                     . "lang/nim")
    (nix-mode                     . "lang/nix")
    (nxml-mode                    . "lang/xml")
    (opascal                      . "lang/opascal")
    (org                          . "lang/org")
    (pascal                       . "lang/pascal")
    (perl-mode                    . "lang/perl")
    (powershell                   . "lang/powershell")
    (processing-mode              . "lang/processing")
    (python-mode                  . "lang/python")
    (qml-mode                     . "lang/qml")
    (racket-mode                  . "lang/racket")
    (ruby-mode                    . "lang/ruby")
    (rust-mode                    . "lang/rust")
    (ssass-mode                   . "lang/sass")
    (scala-mode                   . "lang/scala")
    (scss-mode                    . "lang/scss")
    ((sh-script fish-mode)        . "lang/sh")
    (sql                          . "lang/sql")
    (swift-mode                   . "lang/swift")
    (typescript-mode              . "lang/typescript")
    (verilog-mode                 . "lang/verilog")
    (vhdl-mode                    . "lang/vhdl")
    (vimrc-mode                   . "lang/vimscript")
    (vue-mode                     . "lang/vue")
    ((web-mode sgml-mode)         . "lang/web")
    (yaml-mode                    . "lang/yaml")
    (zig-mode                     . "lang/zig"))
  "Alist of config modules to load.")

(defun jcs-modules-load-entry ()
  "Evaluate through `jcs-module-load-alist' for all required modules."
  (dolist (data jcs-module-load-alist)
    (let ((feats (car data)) (modules (cdr data)))
      (jcs-with-eval-after-load feats (jcs-module-load modules)))))

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
    ("/\\..+ignore\\'"      . gitignore-mode)
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
    ("\\.epub\\'"           . nov-mode)
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

(provide 'jcs-module)
;;; jcs-module.el ends here
