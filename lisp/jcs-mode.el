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
    `(((kbd "M-k")    . kill-buffer-and-window))))

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
  (buffer-disable-undo)
  (goto-address-mode 1)
  (setq truncate-lines nil)

  ;; NOTE: Set smaller font.
  (setq buffer-face-mode-face '(:height 120))
  (buffer-face-mode)

  (jcs-key-local
    `(((kbd "M-k")       . jcs-output-maybe-kill-buffer)
      ((kbd "C-_")       . jcs-output-prev-compilation)
      ((kbd "C-+")       . jcs-output-next-compilation)
      ((kbd "C-S-<f11>") . compilation-previous-error)
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
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next))
      ((kbd "M-k")    . kill-buffer-and-window))))

;;; Tabulated List
(jcs-add-hook 'tabulated-list-mode-hook
  (when (memq major-mode '(Buffer-menu-mode package-menu-mode))
    (buffer-wrap-mode 1)))

;;
;;; Project

(defun jcs-active-project-mode-hook ()
  "Hook runs when there is valid project root."
  (when (jcs-funcall-fboundp #'jcs-project-under-p)
    (global-diff-hl-mode 1)
    (editorconfig-mode 1)
    (jcs--safe-lsp-active)))

;;
;;; Base Mode

(jcs-add-hook '(text-mode-hook prog-mode-hook)
  (jcs-line-numbers-active-by-mode)  ; line numbers

  (when (bound-and-true-p jcs-emacs-startup-directory)  ; only after Emacs startup
    (alt-codes-mode 1)
    (auto-highlight-symbol-mode t)
    (display-fill-column-indicator-mode 1)
    (goto-address-mode 1)
    (when jcs-graphic-p (highlight-indent-guides-mode 1))
    (yas-minor-mode 1)

    (jcs-active-project-mode-hook)))

(jcs-add-hook 'text-mode-hook
  (company-fuzzy-backend-add 'company-kaomoji)

  (jcs-insert-header-if-valid
   '("\\(/\\|\\`\\)[Ll][Ii][Cc][Ee][Nn][Ss][Ee]") 'jcs-ask-insert-license-content
   :interactive t)
  (jcs-insert-header-if-valid
   '("\\(/\\|\\`\\)[Cc][Hh][Aa][Nn][Gg][Ee][-_]*[Ll][Oo][Gg]")
   'jcs-ask-insert-changelog-content
   :interactive t))

(defun jcs-prog-mode-hook ()
  "Programming mode hook."
  (when (bound-and-true-p jcs-emacs-startup-directory)  ; only after Emacs startup
    (unless (jcs-contain-list-type-str "-" (list comment-start comment-end) 'regex)
      (modify-syntax-entry ?- "_"))

    ;; Ensure indentation level is available
    (indent-control-ensure-tab-width)

    (abbrev-mode 1)
    (highlight-numbers-mode 1)))

(add-hook 'prog-mode-hook #'jcs-prog-mode-hook)

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
    (conf-mode               . jcs-properties-mode)
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
    (ini-mode)
    (jayces-mode)
    (jenkinsfile-mode)
    (js2-mode                . jcs-js-mode)
    (json-mode)
    (julia-mode)
    (kotlin-mode)
    (less-css-mode)
    (lua-mode)
    (make-mode               . jcs-makefile-mode)
    (markdown-mode)
    ((masm-mode nasm-mode)   . jcs-asm-mode)
    (nginx-mode)
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
    (rjsx-mode               . jcs-jsx-mode)
    (ruby-mode)
    (rust-mode)
    (ssass-mode              . jcs-sass-mode)
    (scala-mode)
    (scss-mode)
    (sh-script               . jcs-sh-mode)
    (sql                     . jcs-sql-mode)
    (swift-mode)
    (typescript-mode)
    (verilog-mode)
    (vimrc-mode              . jcs-vimscript-mode)
    (vue-mode)
    (web-mode)
    (yaml-mode))
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
    ("\\.as'?\\'"                  . actionscript-mode)
    ("\\.agda'?\\'"                . agda-mode)
    ("\\.applescript'?\\'"         . applescript-mode)
    ("\\.scpt'?\\'"                . applescript-mode)
    ("\\.scptd'?\\'"               . applescript-mode)
;;; B
    ("\\.bas'\\'"                  . basic-mode)
    ("\\.bat'?\\'"                 . bat-mode)
;;; C
    ("\\.hin'?\\'"                 . c++-mode)
    ("\\.cin'?\\'"                 . c++-mode)
    ("\\.cpp'?\\'"                 . c++-mode)
    ("\\.hpp'?\\'"                 . c++-mode)
    ("\\.inl'?\\'"                 . c++-mode)
    ("\\.rdc'?\\'"                 . c++-mode)
    ("\\.cc'?\\'"                  . c++-mode)
    ("\\.c8'?\\'"                  . c++-mode)
    ("\\.h'?\\'"                   . c++-mode)
    ("\\.c'?\\'"                   . c++-mode)
    ("\\.ml[iylp]?$"               . caml-mode)
    ("\\.clj'?\\'"                 . clojure-mode)
    ("\\.cljs'?\\'"                . clojure-mode)
    ("\\.cljc'?\\'"                . clojure-mode)
    ("\\(/\\|\\`\\)CMakeLists.txt" . cmake-mode)
    ("\\.ac'?\\'"                  . cmake-mode)
    ("\\.cbl'?\\'"                 . cobol-mode)
    ("\\.properties'?\\'"          . conf-javaprop-mode)
    ("\\.cs'?\\'"                  . csharp-mode)
    ("\\.css'?"                    . css-mode)
;;; D
    ("\\.dart'?"                   . dart-mode)
    ("\\(/\\|\\`\\)Dokerfile"      . dockerfile-mode)
;;; E
    ("\\.ex'?\\'"                  . elixir-mode)
    ("\\.exs'?\\'"                 . elixir-mode)
    ("\\.el'?\\'"                  . emacs-lisp-mode)
    ("\\.erl'?\\'"                 . erlang-mode)
    ("\\.hrl'?\\'"                 . erlang-mode)
;;; F
    ("\\.fountain'?\\'"            . fountain-mode)
    ("\\.fs'?\\'"                  . fsharp-mode)
;;; G
    ("\\.gen'?\\'"                 . gen-mode)
    ("\\.gd'?\\'"                  . gdscript-mode)
    ("\\.gitattributes'?\\'"       . gitattributes-mode)
    ("\\.gitconfig'?\\'"           . gitconfig-mode)
    ("\\.gitignore'?\\'"           . gitignore-mode)
    ("\\.dockerignore'?\\'"        . gitignore-mode)
    ("\\.npmignore'?\\'"           . gitignore-mode)
    ("\\.unityignore'?\\'"         . gitignore-mode)
    ("\\.vscodeignore'?\\'"        . gitignore-mode)
    ("\\.frag'?\\'"                . glsl-mode)
    ("\\.geom'?\\'"                . glsl-mode)
    ("\\.glsl'?\\'"                . glsl-mode)
    ("\\.vert'?\\'"                . glsl-mode)
    ("\\.go'?\\'"                  . go-mode)
    ("\\.groovy'?\\'"              . groovy-mode)
    ("\\.gradle'?\\'"              . groovy-mode)
;;; H
    ("\\.hs'?\\'"                  . haskell-mode)
    ("\\.hx'?\\'"                  . haxe-mode)
    ("\\.hxml'?\\'"                . haxe-mode)
;;; I
    ("\\.ini'?\\'"                 . ini-mode)
;;; J
    ("\\.java'?\\'"                . java-mode)
    ("\\.jcs'?\\'"                 . jayces-mode)
    ("\\.jayces'?\\'"              . jayces-mode)
    ("Jenkinsfile\\'"              . jenkinsfile-mode)
    ("\\.js'?\\'"                  . js2-mode)
    ("\\.json'?\\'"                . json-mode)
    ("\\.jsx'?\\'"                 . rjsx-mode)
;;; K
    ("\\.kt'?\\'"                  . kotlin-mode)
    ("\\.ktm'?\\'"                 . kotlin-mode)
    ("\\.kts'?\\'"                 . kotlin-mode)
;;; L
    ("\\.less'?\\'"                . less-css-mode)
    ("\\.lisp'?\\'"                . lisp-mode)
    ("\\.lua'?\\'"                 . lua-mode)
    ("\\.luac'?\\'"                . lua-mode)
;;; M
    ("\\.mak'?\\'"                 . makefile-mode)
    ("\\.makfile'?\\'"             . makefile-mode)
    ("\\(/\\|\\`\\)[Mm]akefile"    . makefile-mode)
    ("\\.md'?\\'"                  . markdown-mode)
    ("\\.markdown'?\\'"            . markdown-mode)
    ("\\.asm'?\\'"                 . masm-mode)
    ("\\.inc'?\\'"                 . masm-mode)
;;; N
    ("\\.asm'?\\'"                 . nasm-mode)
    ("\\.inc'?\\'"                 . nasm-mode)
    ("\\.nix'?\\'"                 . nix-mode)
;;; O
    ("\\.m'?\\'"                   . objc-mode)
    ("\\.mm'?\\'"                  . objc-mode)
    ("\\.dpk'?\\'"                 . opascal-mode)
    ("\\.dpr'?\\'"                 . opascal-mode)
    ("\\.org'?\\'"                 . org-mode)
;;; P
    ("\\.pas'?\\'"                 . pascal-mode)
    ("\\.pl'?\\'"                  . perl-mode)
    ("\\.pde'?\\'"                 . processing-mode)
    ("\\.ps1'?\\'"                 . powershell-mode)
    ("\\.py'?\\'"                  . python-mode)
    ("\\.pyc'?\\'"                 . python-mode)
;;; R
    ("\\.r'?\\'"                   . ess-r-mode)
    ("\\.rb'?\\'"                  . ruby-mode)
    ("\\.rs'?\\'"                  . rust-mode)
;;; S
    ("\\.sass'?\\'"                . ssass-mode)
    ("\\.scala'?\\'"               . scala-mode)
    ("\\.scss?\\'"                 . scss-mode)
    ("\\.sh'?\\'"                  . sh-mode)
    ("\\.linux'?\\'"               . sh-mode)
    ("\\.macosx'?\\'"              . sh-mode)
    ("\\.shader'?\\'"              . shader-mode)
    ("\\.sql'?\\'"                 . sql-mode)
    ("\\.swift'?\\'"               . swift-mode)
;;; T
    ("\\.ts'?\\'"                  . typescript-mode)
    ("\\.tsx'?\\'"                 . typescript-mode)
    ("\\.toml'?\\'"                . conf-toml-mode)
;;; V
    ("\\.v'?\\'"                   . verilog-mode)
    ("\\.vim\\(rc\\)'?\\'"         . vimrc-mode)
    ("\\(/\\|\\`\\)_vimrc"         . vimrc-mode)
    ;;
    ;;
    ;;(
    ("\\.vue'?\\'"                 . web-mode)
;;; W
    ("\\.phtml\\'"                 . web-mode)
    ("\\.tpl\\.php\\'"             . web-mode)
    ("\\.erb\\'"                   . web-mode)
    ("\\.mustache\\'"              . web-mode)
    ("\\.djhtml\\'"                . web-mode)
    ("\\.html?\\'"                 . web-mode)
    ("\\.php?\\'"                  . web-mode)
    ("\\.[agj]sp\\'"               . web-mode)
    ;;
    ("\\.as[cp]x\\'"               . web-mode)
    ("\\.cshtml\\'"                . web-mode)
    ("\\.[Mm]aster\\'"             . web-mode)
;;; X
    ("\\.xml'?\\'"                 . nxml-mode)
;;; Y
    ("\\.yaml'?\\'"                . yaml-mode)
    ("\\.yml'?\\'"                 . yaml-mode))
  auto-mode-alist))

(provide 'jcs-mode)
;;; jcs-mode.el ends here
