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

;;;###autoload
(defun jcs-insert-command-mode-toggle ()
  "Toggle command/insert mode."
  (interactive)
  (if (jcs-mode-stats-p 'insert) (jcs-command-mode) (jcs-insert-mode)))

;;;###autoload
(defun jcs-depend-cross-mode-toggle ()
  "Toggle depend/cross mode."
  (interactive)
  (unless (minibufferp)
    (if (jcs-mode-stats-p 'cross) (jcs-depend-mode) (jcs-cross-mode))))

;;;###autoload
(defun jcs-reload-active-mode ()
  "Reload the active mode.
Note this is opposite logic to the toggle mode function."
  (interactive)
  (require 'cl-lib)
  (jcs-mute-apply
    (let ((mode-state jcs-mode--state))
      (jcs-mode-reset-state)
      (cl-case mode-state
        (cross  (jcs-cross-mode))
        (depend (jcs-depend-mode))))))

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
                         (license-templates-names)
                         (jcs-dir-to-filename jcs-license-template-path ".txt"))
                 #'string-lessp)))))
  (let ((lice-path (format "%s%s.txt" jcs-license-template-path in-type)))
    (cond ((string= in-type "Default (empty)") (progn ))
          ((jcs-contain-list-string (license-templates-names) in-type)
           (license-templates-insert in-type))
          (t
           (file-header-insert-template-by-file-path lice-path)))))

;;
;; (@* "Change Log" )
;;

(defun jcs-ask-insert-changelog-content (in-type)
  "Ask to insert the changelog content base on IN-TYPE."
  (interactive
   (list (completing-read
          "Type of the changelog: "
          (append (list "Default (empty)")
                  (jcs-dir-to-filename jcs-changelog-template-path ".txt")))))
  (cond ((string= in-type "Default (empty)")
         ;; Do nothing...
         )
        (t
         (file-header-insert-template-by-file-path
          (format "%s%s.txt" jcs-changelog-template-path in-type)))))

;;
;; (@* "Special Modes" )
;;

;;;###autoload
(defun jcs-command-mode()
  "In command mode."
  (interactive)
  (unless (jcs-mode-stats-p 'command)
    ;; switch to view mode
    ;;(view-mode-enable)

    ;; Customize Mode Line
    (jcs-gray-mode-line)

    ;; Unset insert mode key
    ;; NOTE: unset key should be before of set keys

    ;; Set command mode key

    ;; Update mode state.
    (setq jcs-mode--state 'command)

    (message "[INFO] Turn into `command-mode` now")))

;;;###autoload
(defun jcs-insert-mode()
  "In insert mode."
  (interactive)
  (unless (jcs-mode-stats-p 'insert)
    ;; disable to view mode
    ;;(view-mode-disable)

    ;; Customize Mode Line
    (jcs-dark-green-mode-line)

    ;; Unset command mode key
    ;; NOTE: unset key should be before of set keys

    ;; Set insert mode key

    ;; Update mode state.
    (setq jcs-mode--state 'insert)

    (message "[INFO] Turn into `insert-mode` now")))

;;;###autoload
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
    (define-key global-map (kbd "C-f") #'ivy-searcher-search-file)
    (define-key global-map (kbd "C-S-f") #'ivy-searcher-search-project)

    ;; Update mode state.
    (setq jcs-mode--state 'depend)

    (message "[INFO] Turn into `depend-mode` now")))

;;;###autoload
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
    (define-key global-map (kbd "C-f") #'isearch-forward)
    (define-key global-map (kbd "C-S-f") #'isearch-project-forward)

    ;; Update mode state.
    (setq jcs-mode--state 'cross)

    (message "[INFO] Turn into `cross-mode` now")))

;;----------------------------------------------------------------------------
;;; Startup Modes

;; NOTE: These are modes that will startup immediately, meaning there will
;; be no benefits having in the separated files except the modulation.
;;
;; So just put all the startup modes' configuration here.

;;============================================================================
;; Special

(defun jcs-special-mode-hook ()
  "Hook for `special-mode'."
  (goto-address-mode 1))

(add-hook 'special-mode-hook 'jcs-special-mode-hook)

;;============================================================================
;; Backtrace

(defun jcs-backtrace-mode-hook ()
  "Hook for `backtrace-mode'."
  (buffer-wrap-mode 1))

(add-hook 'backtrace-mode-hook #'jcs-backtrace-mode-hook)

;;============================================================================
;; Diff

(defun jcs-diff-mode-hook ()
  "Hook for `diff-mode'."
  (jcs-bind-key (kbd "M-k") #'jcs-maybe-kill-this-buffer)
  (jcs-bind-key (kbd "M-K") #'jcs-reopen-this-buffer))

(add-hook 'diff-mode-hook #'jcs-diff-mode-hook)

;;============================================================================
;; Compilation

(defun jcs-compilation-mode-hook ()
  "Hook for `compilation-mode'."
  (buffer-disable-undo)
  (goto-address-mode 1)
  (jcs-disable-truncate-lines)

  ;; NOTE: Set smaller font.
  (setq buffer-face-mode-face '(:height 120))
  (buffer-face-mode)

  (jcs-bind-key (kbd "M-k") #'jcs-output-maybe-kill-buffer)
  (jcs-bind-key (kbd "C-_") #'jcs-output-prev-compilation)
  (jcs-bind-key (kbd "C-+") #'jcs-output-next-compilation))

(add-hook 'compilation-mode-hook 'jcs-compilation-mode-hook)
(add-hook 'comint-mode-hook 'jcs-compilation-mode-hook)

;;============================================================================
;; Message Buffer

(defun jcs-message-buffer-mode-hook ()
  "Hook for `message-buffer-mode'."
  (auto-highlight-symbol-mode 1)
  (goto-address-mode 1)
  (page-break-lines-mode 1))

(add-hook 'messages-buffer-mode-hook 'jcs-message-buffer-mode-hook)

;;============================================================================
;; Tabulated List

(defun jcs-tabulated-list-mode-hook ()
  "Hook for `tabulated-list-mode'."
  (when (memq major-mode '(Buffer-menu-mode package-menu-mode))
    (buffer-wrap-mode 1)))

(add-hook 'tabulated-list-mode-hook 'jcs-tabulated-list-mode-hook)

;;============================================================================
;; Project

(defun jcs-active-project-mode-hook ()
  "Hook runs when there is valid project root."
  (when (jcs-project-under-p)
    (editorconfig-mode 1)
    (jcs--safe-lsp-active)))

;;============================================================================
;; Base Mode

(defun jcs-base-mode-hook ()
  "Major mode hook for every major mode."
  (auto-highlight-symbol-mode t)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (highlight-indent-guides-mode 1)
  (origami-indicators-mode 1)

  (jcs-active-project-mode-hook))

(add-hook 'text-mode-hook 'jcs-base-mode-hook)
(add-hook 'prog-mode-hook 'jcs-base-mode-hook)

;;============================================================================
;; Text Mode

(defun jcs-text-mode-hook ()
  "Text mode hook."
  (jcs-insert-header-if-valid '("\\(/\\|\\`\\)[Ll][Ii][Cc][Ee][Nn][Ss][Ee]")
                              'jcs-ask-insert-license-content
                              :interactive t)

  (jcs-insert-header-if-valid '("\\(/\\|\\`\\)[Cc][Hh][Aa][Nn][Gg][Ee][-_]*[Ll][Oo][Gg]")
                              'jcs-ask-insert-changelog-content
                              :interactive t))

(add-hook 'text-mode-hook 'jcs-text-mode-hook)

;;============================================================================
;; Programming Mode

(defconst jcs-mode--dash-major-modes '(elm-mode lua-mode)
  "List of major modes that use dash for commenting.

To avoid syntax highlighting error for comment.")

(defun jcs-prog-mode-hook ()
  "Programming language mode hook."
  (unless (jcs-is-current-major-mode-p jcs-mode--dash-major-modes)
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

(add-hook 'prog-mode-hook 'jcs-prog-mode-hook)

;;============================================================================
;; Emacs Lisp

(defun jcs-emacs-lisp-mode-hook ()
  "Emacs Lisp mode hook."
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word.

  (jcs-insert-header-if-valid '("[.]el")
                              'jcs-insert-emacs-lisp-template))

(add-hook 'emacs-lisp-mode-hook 'jcs-emacs-lisp-mode-hook)

;;============================================================================
;; Lisp

(defun jcs-lisp-mode-hook ()
  "Lisp mode hook."
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word.

  (jcs-insert-header-if-valid '("[.]lisp")
                              'jcs-insert-lisp-template))

(add-hook 'lisp-mode-hook 'jcs-lisp-mode-hook)

;;============================================================================
;; Lisp Interaction

(defun jcs-lisp-interaction-mode-hook ()
  "Lisp Interaction mode hook."
  (jcs-bind-key (kbd "M-k") #'jcs-scratch-buffer-maybe-kill)
  (jcs-bind-key (kbd "M-K") #'jcs-scratch-buffer-refresh))

(add-hook 'lisp-interaction-mode-hook 'jcs-lisp-interaction-mode-hook)

;;============================================================================
;; View

(defun jcs-view-mode-hook ()
  "In view mode, read only file."
  (require 'view)
  (unless (equal jcs-mode--state 'view)
    ;; unset all the key
    (define-key view-mode-map [tab] nil)
    (define-key view-mode-map (kbd "RET") nil)

    (dolist (key-str jcs-key-list)
      (define-key view-mode-map key-str nil))))

(add-hook 'view-mode-hook 'jcs-view-mode-hook)

;;----------------------------------------------------------------------------
;;; Modes

(with-eval-after-load 'message (require 'jcs-message-mode))
(with-eval-after-load 're-builder (require 'jcs-re-builder-mode))
(with-eval-after-load 'shell (require 'jcs-shell-mode))
(with-eval-after-load 'esh-mode (require 'jcs-eshell-mode))
(with-eval-after-load 'yasnippet (require 'jcs-snippet-mode))

(with-eval-after-load 'actionscript-mode (require 'jcs-actionscript-mode))
(with-eval-after-load 'applescript-mode (require 'jcs-applescript-mode))
(jcs-with-eval-after-load-multiple '(masm-mode nasm-mode) (require 'jcs-asm-mode))
(with-eval-after-load 'basic-mode (require 'jcs-basic-mode))
(with-eval-after-load 'bat-mode (require 'jcs-batch-mode))
(with-eval-after-load 'cc-mode
  (require 'jcs-cc-mode)
  (require 'jcs-c-mode)
  (require 'jcs-c++-mode)
  (require 'jcs-java-mode)
  (require 'jcs-objc-mode))
(with-eval-after-load 'clojure-mode (require 'jcs-clojure-mode))
(with-eval-after-load 'cmake-mode (require 'jcs-cmake-mode))
(with-eval-after-load 'cobol-mode (require 'jcs-cobol-mode))
(with-eval-after-load 'conf-mode (require 'jcs-properties-mode))
(with-eval-after-load 'csharp-mode (require 'jcs-csharp-mode))
(with-eval-after-load 'css-mode (require 'jcs-css-mode))
(with-eval-after-load 'dart-mode (require 'jcs-dart-mode))
(with-eval-after-load 'dockerfile-mode (require 'jcs-dockerfile-mode))
(with-eval-after-load 'elixir-mode (require 'jcs-elixir-mode))
(with-eval-after-load 'elm-mode (require 'jcs-elm-mode))
(with-eval-after-load 'erlang (require 'jcs-erlang-mode))
(with-eval-after-load 'ess-r-mode (require 'jcs-r-mode))
(with-eval-after-load 'fountain-mode (require 'jcs-fountain-mode))
(with-eval-after-load 'fsharp-mode (require 'jcs-fsharp-mode))
(with-eval-after-load 'gdscript-mode (require 'jcs-gdscript-mode))
(with-eval-after-load 'gitattributes-mode (require 'jcs-git-mode))
(with-eval-after-load 'gitconfig-mode (require 'jcs-git-mode))
(with-eval-after-load 'gitignore-mode (require 'jcs-git-mode))
(with-eval-after-load 'glsl-mode (require 'jcs-shader-mode))
(with-eval-after-load 'go-mode (require 'jcs-go-mode))
(with-eval-after-load 'groovy-mode (require 'jcs-groovy-mode))
(with-eval-after-load 'haskell-mode (require 'jcs-haskell-mode))
(with-eval-after-load 'haxe-mode (require 'jcs-haxe-mode))
(with-eval-after-load 'ini-mode (require 'jcs-ini-mode))
(with-eval-after-load 'jayces-mode (require 'jcs-jayces-mode))
(with-eval-after-load 'jenkinsfile-mode (require 'jcs-jenkinsfile-mode))
(with-eval-after-load 'js2-mode (require 'jcs-js-mode))
(with-eval-after-load 'json-mode (require 'jcs-json-mode))
(with-eval-after-load 'kotlin-mode (require 'jcs-kotlin-mode))
(with-eval-after-load 'less-css-mode (require 'jcs-less-css-mode))
(with-eval-after-load 'lua-mode (require 'jcs-lua-mode))
(with-eval-after-load 'make-mode (require 'jcs-make-mode))
(with-eval-after-load 'markdown-mode (require 'jcs-markdown-mode))
(with-eval-after-load 'masm-mode (require 'jcs-asm-mode))
(with-eval-after-load 'nasm-mode (require 'jcs-asm-mode))
(with-eval-after-load 'nix-mode (require 'jcs-nix-mode))
(with-eval-after-load 'nxml-mode (require 'jcs-xml-mode))
(with-eval-after-load 'opascal (require 'jcs-opascal-mode))
(with-eval-after-load 'org (require 'jcs-org-mode))
(with-eval-after-load 'pascal (require 'jcs-pascal-mode))
(with-eval-after-load 'perl-mode (require 'jcs-perl-mode))
(with-eval-after-load 'powershell (require 'jcs-powershell-mode))
(with-eval-after-load 'processing-mode (require 'jcs-processing-mode))
(with-eval-after-load 'python-mode (require 'jcs-python-mode))
(with-eval-after-load 'rjsx-mode (require 'jcs-jsx-mode))
(with-eval-after-load 'ruby-mode (require 'jcs-ruby-mode))
(with-eval-after-load 'rust-mode (require 'jcs-rust-mode))
(with-eval-after-load 'ssass-mode (require 'jcs-sass-mode))
(with-eval-after-load 'scala-mode (require 'jcs-scala-mode))
(with-eval-after-load 'scss-mode (require 'jcs-scss-mode))
(with-eval-after-load 'sh-script (require 'jcs-sh-mode))
(with-eval-after-load 'shader-mode (require 'jcs-shader-mode))
(with-eval-after-load 'sql (require 'jcs-sql-mode))
(with-eval-after-load 'swift-mode (require 'jcs-swift-mode))
(with-eval-after-load 'typescript-mode (require 'jcs-typescript-mode))
(with-eval-after-load 'verilog-mode (require 'jcs-verilog-mode))
(with-eval-after-load 'vimrc-mode (require 'jcs-vimscript-mode))
(with-eval-after-load 'vue-mode (require 'jcs-vue-mode))
(with-eval-after-load 'web-mode (require 'jcs-web-mode))
(with-eval-after-load 'yaml-mode (require 'jcs-yaml-mode))


;;;
;; Auto mode Management

(defun jcs-add-auto-mode-alist (pr &optional append)
  "Add a property list to `auto-mode-alist'.

See `add-to-list' function description for argument APPEND."
  (add-to-list 'auto-mode-alist pr append))

;;; A
(progn
  (jcs-add-auto-mode-alist '("\\.as'?\\'" . actionscript-mode))
  (jcs-add-auto-mode-alist '("\\.applescript'?\\'" . applescript-mode))
  (jcs-add-auto-mode-alist '("\\.scpt'?\\'" . applescript-mode))
  (jcs-add-auto-mode-alist '("\\.scptd'?\\'" . applescript-mode)))

;;; B
(progn
  (jcs-add-auto-mode-alist '("\\.bas'\\'" . basic-mode))
  (jcs-add-auto-mode-alist '("\\.bat'?\\'" . bat-mode)))

;;; C
(progn
  (jcs-add-auto-mode-alist '("\\.hin'?\\'" . c++-mode))
  (jcs-add-auto-mode-alist '("\\.cin'?\\'" . c++-mode))
  (jcs-add-auto-mode-alist '("\\.cpp'?\\'" . c++-mode))
  (jcs-add-auto-mode-alist '("\\.hpp'?\\'" . c++-mode))
  (jcs-add-auto-mode-alist '("\\.inl'?\\'" . c++-mode))
  (jcs-add-auto-mode-alist '("\\.rdc'?\\'" . c++-mode))
  (jcs-add-auto-mode-alist '("\\.cc'?\\'" . c++-mode))
  (jcs-add-auto-mode-alist '("\\.c8'?\\'" . c++-mode))
  (jcs-add-auto-mode-alist '("\\.h'?\\'" . c++-mode))
  (jcs-add-auto-mode-alist '("\\.c'?\\'" . c++-mode))
  (jcs-add-auto-mode-alist '("\\.clj'?\\'" . clojure-mode))
  (jcs-add-auto-mode-alist '("\\.cljs'?\\'" . clojure-mode))
  (jcs-add-auto-mode-alist '("\\.cljc'?\\'" . clojure-mode))
  (jcs-add-auto-mode-alist '("\\(/\\|\\`\\)CMakeLists.txt" . cmake-mode))
  ;; NOTE: For autotools, autoconf, automake.
  (jcs-add-auto-mode-alist '("\\.ac'?\\'" . cmake-mode))
  (jcs-add-auto-mode-alist '("\\.cbl'?\\'" . cobol-mode))
  (jcs-add-auto-mode-alist '("\\.properties'?\\'" . conf-javaprop-mode))
  (jcs-add-auto-mode-alist '("\\.cs'?\\'" . csharp-mode))
  (jcs-add-auto-mode-alist '("\\.css'?" . css-mode)))

;;; D
(progn
  (jcs-add-auto-mode-alist '("\\.dart'?" . dart-mode))
  (jcs-add-auto-mode-alist '("\\(/\\|\\`\\)Dokerfile" . dockerfile-mode )))

;;; E
(progn
  (jcs-add-auto-mode-alist '("\\.ex'?\\'" . elixir-mode))
  (jcs-add-auto-mode-alist '("\\.exs'?\\'" . elixir-mode))
  (jcs-add-auto-mode-alist '("\\.el'?\\'" . emacs-lisp-mode))
  (jcs-add-auto-mode-alist '("\\.erl'?\\'" . erlang-mode))
  (jcs-add-auto-mode-alist '("\\.hrl'?\\'" . erlang-mode)))

;;; F
(progn
  (jcs-add-auto-mode-alist '("\\.fountain'?\\'" . fountain-mode))
  (jcs-add-auto-mode-alist '("\\.fs'?\\'" . fsharp-mode)))

;;; G
(progn
  (jcs-add-auto-mode-alist '("\\.gen'?\\'" . gen-mode))

  (jcs-add-auto-mode-alist '("\\.gd'?\\'" . gdscript-mode))

  (jcs-add-auto-mode-alist '("\\.gitattributes'?\\'" . gitattributes-mode))
  (jcs-add-auto-mode-alist '("\\.gitconfig'?\\'" . gitconfig-mode))
  (jcs-add-auto-mode-alist '("\\.gitignore'?\\'" . gitignore-mode))
  (jcs-add-auto-mode-alist '("\\.dockerignore'?\\'" . gitignore-mode))
  (jcs-add-auto-mode-alist '("\\.npmignore'?\\'" . gitignore-mode))
  (jcs-add-auto-mode-alist '("\\.unityignore'?\\'" . gitignore-mode))
  (jcs-add-auto-mode-alist '("\\.vscodeignore'?\\'" . gitignore-mode))

  (jcs-add-auto-mode-alist '("\\.frag'?\\'" . glsl-mode))
  (jcs-add-auto-mode-alist '("\\.geom'?\\'" . glsl-mode))
  (jcs-add-auto-mode-alist '("\\.glsl'?\\'" . glsl-mode))
  (jcs-add-auto-mode-alist '("\\.vert'?\\'" . glsl-mode))

  (jcs-add-auto-mode-alist '("\\.go'?\\'" . go-mode))

  (jcs-add-auto-mode-alist '("\\.groovy'?\\'" . groovy-mode))
  (jcs-add-auto-mode-alist '("\\.gradle'?\\'" . groovy-mode)))

;;; H
(progn
  (jcs-add-auto-mode-alist '("\\.hs'?\\'" . haskell-mode))
  (jcs-add-auto-mode-alist '("\\.hx'?\\'" . haxe-mode))
  (jcs-add-auto-mode-alist '("\\.hxml'?\\'" . haxe-mode)))

;;; I
(progn
  (jcs-add-auto-mode-alist '("\\.ini'?\\'" . ini-mode)))

;;; J
(progn
  (jcs-add-auto-mode-alist '("\\.java'?\\'" . java-mode))
  (jcs-add-auto-mode-alist '("\\.jcs'?\\'" . jayces-mode))
  (jcs-add-auto-mode-alist '("\\.jayces'?\\'" . jayces-mode))
  (jcs-add-auto-mode-alist '("Jenkinsfile\\'" . jenkinsfile-mode))
  (jcs-add-auto-mode-alist '("\\.js'?\\'" . js2-mode))
  (jcs-add-auto-mode-alist '("\\.json'?\\'" . json-mode))
  (jcs-add-auto-mode-alist '("\\.jsx'?\\'" . rjsx-mode)))

;;; K
(progn
  (jcs-add-auto-mode-alist '("\\.kt'?\\'" . kotlin-mode))
  (jcs-add-auto-mode-alist '("\\.ktm'?\\'" . kotlin-mode))
  (jcs-add-auto-mode-alist '("\\.kts'?\\'" . kotlin-mode)))

;;; L
(progn
  (jcs-add-auto-mode-alist '("\\.less'?\\'" . less-css-mode))
  (jcs-add-auto-mode-alist '("\\.lisp'?\\'" . lisp-mode))
  (jcs-add-auto-mode-alist '("\\.lua'?\\'" . lua-mode))
  (jcs-add-auto-mode-alist '("\\.luac'?\\'" . lua-mode)))

;;; M
(progn
  (jcs-add-auto-mode-alist '("\\.mak'?\\'" . makefile-mode))
  (jcs-add-auto-mode-alist '("\\.makfile'?\\'" . makefile-mode))
  (jcs-add-auto-mode-alist '("\\(/\\|\\`\\)[Mm]akefile" . makefile-mode))
  (jcs-add-auto-mode-alist '("\\.md'?\\'" . markdown-mode))
  (jcs-add-auto-mode-alist '("\\.markdown'?\\'" . markdown-mode))

  (jcs-add-auto-mode-alist '("\\.asm'?\\'" . masm-mode))
  (jcs-add-auto-mode-alist '("\\.inc'?\\'" . masm-mode)))

;;; N
(progn
  (jcs-add-auto-mode-alist '("\\.asm'?\\'" . nasm-mode))
  (jcs-add-auto-mode-alist '("\\.inc'?\\'" . nasm-mode))

  (jcs-add-auto-mode-alist '("\\.nix'?\\'" . nix-mode)))

;;; O
(progn
  (jcs-add-auto-mode-alist '("\\.m'?\\'" . objc-mode))
  (jcs-add-auto-mode-alist '("\\.mm'?\\'" . objc-mode))
  (jcs-add-auto-mode-alist '("\\.dpk'?\\'" . opascal-mode))
  (jcs-add-auto-mode-alist '("\\.dpr'?\\'" . opascal-mode))
  (jcs-add-auto-mode-alist '("\\.org'?\\'" . org-mode)))

;;; P
(progn
  (jcs-add-auto-mode-alist '("\\.pas'?\\'" . pascal-mode))
  (jcs-add-auto-mode-alist '("\\.pl'?\\'" . perl-mode))
  (jcs-add-auto-mode-alist '("\\.pde'?\\'" . processing-mode))
  (jcs-add-auto-mode-alist '("\\.ps1'?\\'" . powershell-mode))
  (jcs-add-auto-mode-alist '("\\.py'?\\'" . python-mode))
  (jcs-add-auto-mode-alist '("\\.pyc'?\\'" . python-mode)))

;;; R
(progn
  (jcs-add-auto-mode-alist '("\\.r'?\\'" . ess-r-mode))
  (jcs-add-auto-mode-alist '("\\.rb'?\\'" . ruby-mode))
  (jcs-add-auto-mode-alist '("\\.rs'?\\'" . rust-mode)))

;;; S
(progn
  (jcs-add-auto-mode-alist '("\\.sass'?\\'" . ssass-mode))
  (jcs-add-auto-mode-alist '("\\.scala'?\\'" . scala-mode))
  (jcs-add-auto-mode-alist '("\\.scss?\\'" . scss-mode))
  (jcs-add-auto-mode-alist '("\\.sh'?\\'" . sh-mode))
  (jcs-add-auto-mode-alist '("\\.linux'?\\'" . sh-mode))
  (jcs-add-auto-mode-alist '("\\.macosx'?\\'" . sh-mode))
  (jcs-add-auto-mode-alist '("\\.shader'?\\'" . shader-mode))
  (jcs-add-auto-mode-alist '("\\.sql'?\\'" . sql-mode))
  (jcs-add-auto-mode-alist '("\\.swift'?\\'" . swift-mode)))

;;; T
(progn
  (jcs-add-auto-mode-alist '("\\.ts'?\\'" . typescript-mode))
  (jcs-add-auto-mode-alist '("\\.tsx'?\\'" . typescript-mode))
  (jcs-add-auto-mode-alist '("\\.toml'?\\'" . conf-toml-mode))
  (jcs-add-auto-mode-alist '("\\.txt'?\\'" . text-mode) t))

;;; V
(progn
  (jcs-add-auto-mode-alist '("\\.v'?\\'" . verilog-mode))
  (jcs-add-auto-mode-alist '("\\.vim\\(rc\\)'?\\'" . vimrc-mode))
  (jcs-add-auto-mode-alist '("\\(/\\|\\`\\)_vimrc" . vimrc-mode))
  ;; ATTENTION: I think `vue-mode' using `mmm-mode' isn't
  ;; ready for the development yet. So I'm currently using
  ;; `web-mode' instead.
  (progn
    ;;(jcs-add-auto-mode-alist '("\\.vue'?\\'" . vue-mode))
    (jcs-add-auto-mode-alist '("\\.vue'?\\'" . web-mode))))

;;; W
(progn
  (jcs-add-auto-mode-alist '("\\.phtml\\'" . web-mode))
  (jcs-add-auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (jcs-add-auto-mode-alist '("\\.erb\\'" . web-mode))
  (jcs-add-auto-mode-alist '("\\.mustache\\'" . web-mode))
  (jcs-add-auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (jcs-add-auto-mode-alist '("\\.html?\\'" . web-mode))
  (jcs-add-auto-mode-alist '("\\.php?\\'" . web-mode))
  (jcs-add-auto-mode-alist '("\\.[agj]sp\\'" . web-mode))

  ;; ASP .NET
  (jcs-add-auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (jcs-add-auto-mode-alist '("\\.cshtml\\'" . web-mode))
  (jcs-add-auto-mode-alist '("\\.[Mm]aster\\'" . web-mode)))

;;; X
(progn
  (jcs-add-auto-mode-alist '("\\.xml'?\\'" . nxml-mode)))

;;; Y
(progn
  (jcs-add-auto-mode-alist '("\\.yaml'?\\'" . yaml-mode))
  (jcs-add-auto-mode-alist '("\\.yml'?\\'" . yaml-mode)))

(provide 'jcs-mode)
;;; jcs-mode.el ends here
