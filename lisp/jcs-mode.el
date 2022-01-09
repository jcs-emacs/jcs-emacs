;;; jcs-mode.el --- Self mode defines  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Mode State" )
;;

(defvar jcs-mode--state nil
  "Record the state of the current mode.")

(defun jcs-mode-reset-state ()
  "Reset mode state."
  (setq jcs-mode--state nil))

(defun jcs-mode-stats-p (state)
  "Check mode STATE."
  (equal jcs-mode--state state))

(defun jcs-depend-cross-mode-toggle ()
  "Toggle depend/cross mode."
  (interactive)
  (unless (minibufferp)
    (if (jcs-mode-stats-p 'cross) (jcs-depend-mode) (jcs-cross-mode))))

(defun jcs-reload-active-mode ()
  "Reload the active mode.
Note this is opposite logic to the toggle mode function."
  (interactive)
  (jcs-mute-apply
    (let ((mode-state jcs-mode--state))
      (jcs-mode-reset-state)
      (cl-case mode-state
        (`cross  (jcs-cross-mode))
        (`depend (jcs-depend-mode))))))

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

(defun jcs-ask-insert-license-content (in-type)
  "Ask to insert the license content base on IN-TYPE."
  (interactive
   (list (completing-read
          (format "Type of the license: "
                  (progn  ; Preloading for `interactive` function.
                    (require 'license-templates) (require 'subr-x)))
          (delete-dups
           (sort (append (list "Default (empty)")
                         (license-templates-names))
                 #'string-lessp)))))
  (cond ((string= in-type "Default (empty)") (progn ))
        ((jcs-contain-list-string (license-templates-names) in-type)
         (license-templates-insert in-type))))

;;
;; (@* "Change Log" )
;;

(defun jcs-ask-insert-changelog-content (in-type)
  "Ask to insert the changelog content base on IN-TYPE."
  (interactive
   (list (completing-read
          "Type of the changelog: "
          (append (list "Default (empty)")
                  (jcs-dir-to-filename jcs-changelog-template-dir ".txt")))))
  (pcase in-type
    ("Default (empty)" )  ; Do nothing...
    (_
     (file-header-insert-template-by-file-path
      (format "%s%s.txt" jcs-changelog-template-dir in-type)))))

;;
;; (@* "Special Modes" )
;;

(defun jcs-depend-mode ()
  "This mode depend on my own machine. More feature and more control of the editor."
  (interactive)
  (unless (jcs-mode-stats-p 'depend)
    ;; Customize Mode Line
    (jcs-gray-mode-line)

    ;; Unset 'depend' mode key
    ;; NOTE: unset key should be before of set keys
    (global-unset-key (kbd "C-f"))
    (global-unset-key (kbd "C-r"))

    ;; Set 'depend' mode key

    ;; search
    (jcs-key global-map
      `(((kbd "C-f")   . ivy-searcher-search-file)
        ((kbd "C-S-f") . ivy-searcher-search-project)))

    ;; Update mode state.
    (setq jcs-mode--state 'depend)

    (message "[INFO] Turn into `depend-mode` now")))

(defun jcs-cross-mode ()
  "This mode run anywhere will work, usually less powerful then `jcs-depend-mode'."
  (interactive)
  (unless (jcs-mode-stats-p 'cross)
    ;; Customize Mode Line
    (jcs-dark-green-mode-line)

    ;; Unset 'cross' mode key
    ;; NOTE: unset key should be before of set keys
    (global-unset-key (kbd "C-f"))
    (global-unset-key (kbd "C-r"))
    (global-unset-key (kbd "C-r p"))

    ;; Set 'cross' mode key

    ;; search
    (jcs-key global-map
      `(((kbd "C-f")   . isearch-forward)
        ((kbd "C-S-f") . isearch-project-forward)))

    ;; Update mode state.
    (setq jcs-mode--state 'cross)

    (message "[INFO] Turn into `cross-mode` now")))

;;----------------------------------------------------------------------------
;;; Startup Modes

;; NOTE: These are modes that will startup immediately, meaning there will
;; be no benefits having in the separated files except the modulation.
;;
;; So just put all the startup modes' configuration here.

;;; Special
(jcs-add-hook 'special-mode-hook (goto-address-mode 1))

;;; Backtrace
(jcs-add-hook 'backtrace-mode-hook (buffer-wrap-mode 1))

;;; Buffer Menu
(jcs-add-hook 'Buffer-menu-mode-hook (require 'jcs-buffer-menu))

;;; Diff
(jcs-add-hook 'diff-mode-hook
  (jcs-key-local
    `(((kbd "M-k") . jcs-maybe-kill-this-buffer)
      ((kbd "M-K") . jcs-reopen-this-buffer))))

;;; Compilation
(jcs-add-hook '(compilation-mode-hook comint-mode-hook)
  (buffer-disable-undo)
  (goto-address-mode 1)
  (toggle-truncate-lines -1)

  ;; NOTE: Set smaller font.
  (setq buffer-face-mode-face '(:height 120))
  (buffer-face-mode)

  (jcs-key-local
    `(((kbd "M-k") . jcs-output-maybe-kill-buffer)
      ((kbd "C-_") . jcs-output-prev-compilation)
      ((kbd "C-+") . jcs-output-next-compilation))))

;;; Message Buffer
(jcs-add-hook 'messages-buffer-mode-hook
  (auto-highlight-symbol-mode 1)
  (goto-address-mode 1)
  (page-break-lines-mode 1))

;;; Tabulated List
(jcs-add-hook 'tabulated-list-mode-hook
  (when (memq major-mode '(Buffer-menu-mode package-menu-mode))
    (buffer-wrap-mode 1)))

;;============================================================================
;; Project

(defun jcs-active-project-mode-hook ()
  "Hook runs when there is valid project root."
  (when (jcs-project-under-p)
    (global-diff-hl-mode 1)
    (editorconfig-mode 1)
    (jcs--safe-lsp-active)))

;;============================================================================
;; Base Mode

(jcs-add-hook '(text-mode-hook prog-mode-hook)
  (auto-highlight-symbol-mode t)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (when (display-graphic-p) (highlight-indent-guides-mode 1))

  (jcs-active-project-mode-hook))

;;; Text
(jcs-add-hook 'text-mode-hook
  (jcs-insert-header-if-valid '("\\(/\\|\\`\\)[Ll][Ii][Cc][Ee][Nn][Ss][Ee]")
                              'jcs-ask-insert-license-content
                              :interactive t)
  (jcs-insert-header-if-valid '("\\(/\\|\\`\\)[Cc][Hh][Aa][Nn][Gg][Ee][-_]*[Ll][Oo][Gg]")
                              'jcs-ask-insert-changelog-content
                              :interactive t))

;;============================================================================
;; Programming Mode

(defconst jcs-mode--dash-major-modes '(elm-mode lua-mode)
  "List of major modes that use dash for commenting.

To avoid syntax highlighting error for comment.")

(defun jcs-prog-mode-hook ()
  "Programming mode hook."
  (unless (memq major-mode jcs-mode--dash-major-modes)
    (modify-syntax-entry ?- "_"))

  ;; Load Docstring faces.
  (docstr-faces-apply)

  ;; Ensure indentation level is available
  (indent-control-ensure-tab-width)

  ;; Smart Parenthesis
  (dolist (key jcs-smart-closing-parens)
    (jcs-key-advice-add key :around #'jcs-smart-closing))

  (abbrev-mode 1)
  (display-fill-column-indicator-mode 1)
  (highlight-numbers-mode 1))

(add-hook 'prog-mode-hook #'jcs-prog-mode-hook)

;;; Emacs Lisp
(jcs-add-hook 'emacs-lisp-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word.
  (jcs-insert-header-if-valid '("[.]el")
                              'jcs-insert-emacs-lisp-template))

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

;;============================================================================
;; View

(jcs-add-hook 'view-mode-hook
  (require 'view)
  (unless (equal jcs-mode--state 'view)
    ;; unset all the key
    (jcs-key view-mode-map
      `(([tab] . nil)
        ((kbd "RET") . nil)))

    (dolist (key-str jcs-key-list)
      (define-key view-mode-map key-str nil))))

;;----------------------------------------------------------------------------
;;; Modes

(defconst jcs-mode-load-alist
  '(
;;; Others
    (message                 . jcs-message-mode)
    (re-builder              . jcs-re-builder-mode)
    ((shell esh-mode)        . jcs-shell-mode)
    (yasnippet               . jcs-snippet-mode)
;;; Languages
    (actionscript-mode)
    (ada-mode)
    (agda-mode)
    (applescript-mode)
    ((masm-mode nasm-mode)   . jcs-asm-mode)
    (basic-mode)
    (bat-mode                . jcs-batch-mode)
    (cc-mode                 . (jcs-cc-mode
                                jcs-c-mode
                                jcs-c++-mode
                                jcs-java-mode
                                jcs-objc-mode))
    (clojure-mode)
    (cmake-mode)
    (cobol-mode)
    (conf-mode               . jcs-properties-mode)
    (csharp-mode)
    (css-mode)
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
    ((glsl-mode shader-mode) . jcs-shader-mode)
    (go-mode)
    (groovy-mode)
    (haskell-mode)
    (haxe-mode)
    (ini-mode)
    (jayces-mode)
    (jenkinsfile-mode)
    (js2-mode                . jcs-js-mode)
    (json-mode)
    (kotlin-mode)
    (less-css-mode)
    (lua-mode)
    (make-mode)
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
    (let ((mode (car data)) (modules (cdr data)))
      (cond ((listp mode)
             (jcs-with-eval-after-load-multiple mode (jcs-require modules)))
            (t
             (unless modules (setq modules (intern (format "jcs-%s" mode))))
             (with-eval-after-load mode (jcs-require modules)))))))

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
    ("\\.txt'?\\'"                 . text-mode)
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
