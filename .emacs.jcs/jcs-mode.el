;;; jcs-mode.el --- Self mode defines.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;---------------------------------------------
;; Trigger between command and insert mode
;;---------------------------------------------

;;;###autoload
(defun jcs-insert-command-mode-toggle()
  "Toggle command/insert mode."
  (interactive)
  (if (get 'jcs-insert-command-mode-toggle 'state)
      (progn
        (jcs-command-mode)
        (put 'jcs-insert-command-mode-toggle 'state nil))
    (jcs-insert-mode)
    (put 'jcs-insert-command-mode-toggle 'state t)))

;;;###autoload
(defun jcs-depend-cross-mode-toggle()
  "Toggle depend/cross mode."
  (interactive)
  (unless (minibufferp)
    (if (get 'jcs-depend-cross-mode-toggle 'state)
        (jcs-depend-mode)
      (jcs-cross-mode))))

;;;###autoload
(defun jcs-reload-active-mode ()
  "Reload the active mode.
Note this is opposite logic to the toggle mode function."
  (interactive)
  (if (get 'jcs-depend-cross-mode-toggle 'state)
      (jcs-cross-mode)   ; if state is true keep on cross mode.
    (jcs-depend-mode)))  ; vice versa, keep on depend mode.

(defun jcs-reload-active-mode-error-handle ()
  "Reload the active by handling the error occurrence."
  (unless (minibufferp)
    (if (jcs-backtrace-occurs-p)
        (jcs-red-mode-line)  ; When error, use red mode line.
      (jcs-reload-active-mode))))

(defun jcs-set-tab-width-by-mode (tw)
  "Set the tab width TW for current major mode."
  (cond
   ((jcs-is-current-major-mode-p '("actionscript-mode"))
    (setq actionscript-indent-level tw))
   ((jcs-is-current-major-mode-p '("cc-mode"
                                   "c-mode"
                                   "c++-mode"
                                   "csharp-mode"
                                   "java-mode"
                                   "jayces-mode"
                                   "objc-mode"))
    (setq c-basic-offset tw))
   ((jcs-is-current-major-mode-p '("css-mode"
                                   "less-css-mode"
                                   "scss-mode"))
    (setq css-indent-offset tw))
   ((jcs-is-current-major-mode-p '("ssass-mode"))
    (setq ssass-tab-width tw))
   ((jcs-is-current-major-mode-p '("js-mode"
                                   "json-mode"))
    (setq js-indent-level tw))
   ((jcs-is-current-major-mode-p '("js2-mode"))
    (setq js2-basic-offset tw))
   ((jcs-is-current-major-mode-p '("lisp-mode"
                                   "lisp-interaction-mode"
                                   "emacs-lisp-mode"))
    (setq lisp-body-indent tw))
   ((jcs-is-current-major-mode-p '("lua-mode"))
    (setq lua-indent-level tw))
   ((jcs-is-current-major-mode-p '("lua-mode"))
    (setq lua-indent-level tw))
   ((jcs-is-current-major-mode-p '("nasm-mode"))
    (setq nasm-basic-offset tw))
   ((jcs-is-current-major-mode-p '("nxml-mode"))
    (setq nxml-child-indent tw))
   ((jcs-is-current-major-mode-p '("python-mode"))
    (setq py-indent-offset tw))
   ((jcs-is-current-major-mode-p '("ruby-mode"))
    (setq ruby-indent-level tw))
   ((jcs-is-current-major-mode-p '("rust-mode"))
    (setq rust-indent-offset tw))
   ((jcs-is-current-major-mode-p '("shader-mode"))
    (setq shader-indent-offset tw))
   ((jcs-is-current-major-mode-p '("sql-mode"))
    (setq sql-indent-offset tw))
   ((jcs-is-current-major-mode-p '("typescript-mode"))
    (setq typescript-indent-level tw))
   ((jcs-is-current-major-mode-p '("web-mode"))
    (setq web-mode-markup-indent-offset tw)
    (setq web-mode-css-indent-offset tw)
    (setq web-mode-code-indent-offset tw))
   ((jcs-is-current-major-mode-p '("yaml-mode"))
    (setq yaml-indent-offset tw))
   (t
    (setq tab-width tw)))
  (if tw
      (progn
        (jcs-set-tab-width-record-by-mode tw)
        (message "Current indent level: %s" tw))
    (message "No indent offset defined in major mode: %s" major-mode)))

(defun jcs-get-tab-width-by-mode ()
  "Get indentation level by mode."
  (cond
   ((jcs-is-current-major-mode-p '("actionscript-mode"))
    actionscript-indent-level)
   ((jcs-is-current-major-mode-p '("cc-mode"
                                   "c-mode"
                                   "c++-mode"
                                   "csharp-mode"
                                   "java-mode"
                                   "jayces-mode"
                                   "objc-mode"))
    c-basic-offset)
   ((jcs-is-current-major-mode-p '("css-mode"
                                   "less-css-mode"
                                   "scss-mode"))
    css-indent-offset)
   ((jcs-is-current-major-mode-p '("ssass-mode"))
    ssass-tab-width)
   ((jcs-is-current-major-mode-p '("js-mode"
                                   "json-mode"))
    js-indent-level)
   ((jcs-is-current-major-mode-p '("js2-mode"))
    js2-basic-offset)
   ((jcs-is-current-major-mode-p '("lisp-mode"
                                   "lisp-interaction-mode"
                                   "emacs-lisp-mode"))
    lisp-body-indent)
   ((jcs-is-current-major-mode-p '("lua-mode"))
    lua-indent-level)
   ((jcs-is-current-major-mode-p '("nasm-mode"))
    nasm-basic-offset)
   ((jcs-is-current-major-mode-p '("nxml-mode"))
    nxml-child-indent)
   ((jcs-is-current-major-mode-p '("python-mode"))
    py-indent-offset)
   ((jcs-is-current-major-mode-p '("ruby-mode"))
    ruby-indent-level)
   ((jcs-is-current-major-mode-p '("rust-mode"))
    rust-indent-offset)
   ((jcs-is-current-major-mode-p '("shader-mode"))
    shader-indent-offset)
   ((jcs-is-current-major-mode-p '("sql-mode"))
    sql-indent-offset)
   ((jcs-is-current-major-mode-p '("typescript-mode"))
    typescript-indent-level)
   ((jcs-is-current-major-mode-p '("web-mode"))
    web-mode-markup-indent-offset)
   ((jcs-is-current-major-mode-p '("yaml-mode"))
    yaml-indent-offset)
   (t tab-width)))

(defun jcs-ensure-valid-tab-width (cv dv)
  "Change tab width by current value CV and delta value DV."
  (jcs-clamp-integer (+ cv dv) 0 8))

(defun jcs-delta-tab-width (dv)
  "Increase/Decrease tab width by delta value DV."
  (jcs-set-tab-width-by-mode (jcs-ensure-valid-tab-width (jcs-get-tab-width-by-mode) dv)))

(defun jcs-set-tab-width-record-by-mode (tw &optional mn)
  "Set the tab width record by mode-name MN with tab width TW."
  (unless mn (setq mn (jcs-current-major-mode)))
  (let ((index 0)
        (break-it nil))
    (while (and (< index (length jcs-tab-with-records))
                (not break-it))
      (let ((record-mode-name (car (nth index jcs-tab-with-records))))
        (when (equal mn record-mode-name)
          (setf (cdr (nth index jcs-tab-with-records)) tw)
          (setq break-it t)))
      (setq index (1+ index)))
    (unless break-it
      (message "Tab width record not found: %s" mn))))

(defun jcs-get-tab-width-record-by-mode (&optional mn)
  "Get the tab width record by mode name, MN."
  (unless mn (setq mn (jcs-current-major-mode)))
  (let ((index 0)
        (break-it nil)
        ;; Have default to `tab-width'.
        (target-tab-width tab-width))
    (while (and (< index (length jcs-tab-with-records))
                (not break-it))
      (let ((record-mode-name (car (nth index jcs-tab-with-records)))
            (record-tab-width (cdr (nth index jcs-tab-with-records))))
        (when (equal mn record-mode-name)
          (setq target-tab-width record-tab-width)
          (setq break-it t)))
      (setq index (1+ index)))
    target-tab-width))

(defun jcs-continue-with-tab-width-record ()
  "Keep the tab width the same as last time modified."
  (jcs-set-tab-width-by-mode (jcs-get-tab-width-record-by-mode)))

(defun jcs-buffer-spaces-to-tabs ()
  "Check if buffer using spaces or tabs."
  (if (= (how-many "^\t" (point-min) (point-max)) 0)
      "SPC"
    "TAB"))

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

;;----------------------------------------------
;; License

(defun jcs-ask-insert-license-content (in-type)
  "Ask to insert the license content base on IN-TYPE."
  (interactive (list (completing-read
                      "Type of the license: "
                      (append (list "Default (empty)")
                              (jcs-dir-to-filename jcs-license-template-path ".txt")))))
  (cond ((string= in-type "Default (empty)")
         ;; Do nothing...
         )
        (t
         (file-header-insert-template-by-file-path
          (format "%s%s.txt" jcs-license-template-path in-type)))))

;;----------------------------------------------
;; Change Log

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

;;------------------------------------------------------------------------------------------------------
;;; Command Mode & Insert Mode
;;------------------------------------------------------------------------------------------------------

;;;###autoload
(defun jcs-command-mode()
  "In command mode."
  (interactive)
  (put 'jcs-insert-command-mode-toggle 'state nil)  ; set trigger

  ;; switch to view mode
  ;;(view-mode-enable)

  ;; Customize Mode Line
  (jcs-gray-mode-line)

  ;; Unset insert mode key
  ;; NOTE: unset key should be before of set keys

  ;; Set command mode key

  (message "[INFO] Turn into `command-mode` now"))

;;;###autoload
(defun jcs-insert-mode()
  "In insert mode."
  (interactive)
  (put 'jcs-insert-command-mode-toggle 'state t)  ; set trigger

  ;; disable to view mode
  ;;(view-mode-disable)

  ;; Customize Mode Line
  (jcs-dark-green-mode-line)

  ;; Unset command mode key
  ;; NOTE: unset key should be before of set keys

  ;; Set insert mode key

  (message "[INFO] Turn into `insert-mode` now"))

(defun jcs-view-mode-hook()
  "In view mode, read only file."
  (require 'view)

  ;; unset all the key
  (define-key view-mode-map [tab] nil)
  (define-key view-mode-map (kbd "RET") nil)

  (dolist (key-str jcs-key-list)
    (define-key view-mode-map key-str nil))

  ;; just save buffer, don't care about the tab or spaces.
  (define-key view-mode-map (kbd "C-s") #'save-buffer))

(add-hook 'view-mode-hook 'jcs-view-mode-hook)


;;------------------------------------------------------------------------------------------------------
;;; Local Mode & Online Mode
;;------------------------------------------------------------------------------------------------------

;;;###autoload
(defun jcs-depend-mode ()
  "This mode depend on my own machine. More feature and more control of the editor."
  (interactive)
  ;; Customize Mode Line
  (jcs-gray-mode-line)

  ;; Unset 'depend' mode key
  ;; NOTE: unset key should be before of set keys
  (global-unset-key (kbd "C-f"))
  (global-unset-key (kbd "C-r"))

  ;; Set 'depend' mode key

  ;; search
  (define-key global-map (kbd "C-f") #'counsel-ag)
  (define-key global-map (kbd "C-S-f") #'counsel-ag)  ; TODO: Use `ag' across project?

  (define-key global-map (kbd "C-r p") #'jcs-ag-project-regexp)

  (when (get 'jcs-depend-cross-mode-toggle 'state)
    (message "[INFO] Turn into `depend-mode` now"))

  ;; set toggle trigger
  (put 'jcs-depend-cross-mode-toggle 'state nil))

;;;###autoload
(defun jcs-cross-mode ()
  "This mode run anywhere will work, usually less powerful then `jcs-depend-mode'."
  (interactive)
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

  (unless (get 'jcs-depend-cross-mode-toggle 'state)
    (message "[INFO] Turn into `cross-mode` now"))

  ;; set toggle trigger
  (put 'jcs-depend-cross-mode-toggle 'state t))


;;------------------------------------------------------------------------------------------------------
;;; Startup Modes
;;------------------------------------------------------------------------------------------------------

;; NOTE: These are modes that will startup immediately, meaning there will
;; be no benefits having in the separated files except the modulation.
;;
;; So just put all the startup modes' configuration here.

;;==============================
;;       Compilation
;;------------------------

(defun jcs-compilation-mode-hook ()
  "Compilation mode hook."
  (jcs-disable-truncate-lines)

  ;; NOTE: Set smaller font.
  (setq buffer-face-mode-face '(:height 120))
  (buffer-face-mode)

  (define-key compilation-mode-map (kbd "C-_") #'jcs-output-prev-compilation)
  (define-key compilation-mode-map (kbd "C-+") #'jcs-output-next-compilation))

(add-hook 'compilation-mode-hook 'jcs-compilation-mode-hook)

;;==============================
;;        Text Mode
;;------------------------

(defun jcs-text-mode-hook ()
  "Text mode hook."
  (auto-highlight-symbol-mode t)
  (goto-address-mode 1)

  (jcs-insert-header-if-valid '("\\(/\\|\\`\\)[Ll][Ii][Cc][Ee][Nn][Ss][Ee]")
                              'jcs-ask-insert-license-content
                              t)

  (jcs-insert-header-if-valid '("\\(/\\|\\`\\)[Cc][Hh][Aa][Nn][Gg][Ee][-_]*[Ll][Oo][Gg]")
                              'jcs-ask-insert-changelog-content
                              t))

(add-hook 'text-mode-hook 'jcs-text-mode-hook)

;;==============================
;;    Programming Mode
;;------------------------

(defun jcs-prog-mode-hook ()
  "Programming language mode hook."
  (jcs-mute-apply #'jcs-continue-with-tab-width-record)

  (abbrev-mode 1)
  (auto-highlight-symbol-mode t)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (highlight-indent-guides-mode 1)
  (unless (string= (buffer-name) "*scratch*") (lsp-deferred)))

(add-hook 'prog-mode-hook 'jcs-prog-mode-hook)

;;==============================
;;      Emacs Lisp
;;------------------------

(defun jcs-emacs-lisp-mode-hook ()
  "Emacs Lisp mode hook."
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word.

  (jcs-make-electric-pair-pairs-local '((?\` . ?\')))

  (jcs-insert-header-if-valid '("[.]el")
                              'jcs-insert-emacs-lisp-template))

(add-hook 'emacs-lisp-mode-hook 'jcs-emacs-lisp-mode-hook)


;;==============================
;;          Lisp
;;------------------------

(defun jcs-lisp-mode-hook ()
  "Lisp mode hook."
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word.

  (jcs-make-electric-pair-pairs-local '((?\` . ?\')))

  (jcs-insert-header-if-valid '("[.]lisp")
                              'jcs-insert-lisp-template))

(add-hook 'lisp-mode-hook 'jcs-lisp-mode-hook)

;;==============================
;;     Lisp Interaction
;;------------------------

(defun jcs-lisp-interaction-mode-hook ()
  "Lisp Interaction mode hook."
  (define-key lisp-interaction-mode-map (kbd "M-k") #'jcs-scratch-buffer-maybe-kill)
  (define-key lisp-interaction-mode-map (kbd "M-K") #'jcs-scratch-buffer-refresh))

(add-hook 'lisp-interaction-mode-hook 'jcs-lisp-interaction-mode-hook)


;;------------------------------------------------------------------------------------------------------
;;; Modes
;;------------------------------------------------------------------------------------------------------

(with-eval-after-load 'message (require 'jcs-message-mode))
(with-eval-after-load 're-builder (require 'jcs-re-builder-mode))
(with-eval-after-load 'shell (require 'jcs-shell-mode))
(with-eval-after-load 'esh-mode (require 'jcs-esh-mode))
(with-eval-after-load 'yasnippet (require 'jcs-snippet-mode))

(with-eval-after-load 'actionscript-mode (require 'jcs-actionscript-mode))
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
(with-eval-after-load 'erlang (require 'jcs-erlang-mode))
(with-eval-after-load 'gdscript-mode (require 'jcs-gdscript-mode))
(with-eval-after-load 'gitattributes-mode (require 'jcs-git-mode))
(with-eval-after-load 'gitconfig-mode (require 'jcs-git-mode))
(with-eval-after-load 'gitignore-mode (require 'jcs-git-mode))
(with-eval-after-load 'glsl-mode (require 'jcs-shader-mode))
(with-eval-after-load 'go-mode (require 'jcs-go-mode))
(with-eval-after-load 'haskell-mode (require 'jcs-haskell-mode))
(with-eval-after-load 'haxe-mode (require 'jcs-haxe-mode))
(with-eval-after-load 'ini-mode (require 'jcs-ini-mode))
(with-eval-after-load 'jayces-mode (require 'jcs-jayces-mode))
(with-eval-after-load 'js2-mode (require 'jcs-js-mode))
(with-eval-after-load 'json-mode (require 'jcs-json-mode))
(with-eval-after-load 'kotlin-mode (require 'jcs-kotlin-mode))
(with-eval-after-load 'less-css-mode (require 'jcs-less-css-mode))
(with-eval-after-load 'lua-mode (require 'jcs-lua-mode))
(with-eval-after-load 'make-mode (require 'jcs-make-mode))
(with-eval-after-load 'markdown-mode (require 'jcs-markdown-mode))
(with-eval-after-load 'nasm-mode (require 'jcs-nasm-mode))
(with-eval-after-load 'nxml-mode (require 'jcs-xml-mode))
(with-eval-after-load 'opascal (require 'jcs-opascal-mode))
(with-eval-after-load 'org (require 'jcs-org-mode))
(with-eval-after-load 'pascal (require 'jcs-pascal-mode))
(with-eval-after-load 'perl-mode (require 'jcs-perl-mode))
(with-eval-after-load 'processing-mode (require 'jcs-processing-mode))
(with-eval-after-load 'python-mode (require 'jcs-python-mode))
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

(defun jcs-add-auto-mode-alist (pr)
  "Add mode to alist.
PR : pair file `regexp' and file mode `symbol'."
  (add-to-list 'auto-mode-alist pr))

;;; A
(progn
  (jcs-add-auto-mode-alist '("\\.as'?\\'" . actionscript-mode)))

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
  ;;(jcs-add-auto-mode-alist '("\\.h'?\\'" . c-mode))
  (jcs-add-auto-mode-alist '("\\.c'?\\'" . c-mode))
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

;;; G
(progn
  (jcs-add-auto-mode-alist '("\\.gd'?\\'" . gdscript-mode))

  (jcs-add-auto-mode-alist '("\\.gitattributes'?\\'" . gitattributes-mode))
  (jcs-add-auto-mode-alist '("\\.gitconfig'?\\'" . gitconfig-mode))
  (jcs-add-auto-mode-alist '("\\.gitignore'?\\'" . gitignore-mode))

  (jcs-add-auto-mode-alist '("\\.frag'?\\'" . glsl-mode))
  (jcs-add-auto-mode-alist '("\\.geom'?\\'" . glsl-mode))
  (jcs-add-auto-mode-alist '("\\.glsl'?\\'" . glsl-mode))
  (jcs-add-auto-mode-alist '("\\.vert'?\\'" . glsl-mode))
  (jcs-add-auto-mode-alist '("\\.go'?\\'" . go-mode)))

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
  (jcs-add-auto-mode-alist '("\\.js'?\\'" . js2-mode))
  (jcs-add-auto-mode-alist '("\\.json'?\\'" . json-mode)))

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
  (jcs-add-auto-mode-alist '("\\.md'?\\'" . markdown-mode)))

;;; N
(progn
  (jcs-add-auto-mode-alist '("\\.asm'?\\'" . nasm-mode))
  (jcs-add-auto-mode-alist '("\\.inc'?\\'" . nasm-mode)))

;;; O
(progn
  (jcs-add-auto-mode-alist '("\\.m'?\\'" . objc-mode))
  (jcs-add-auto-mode-alist '("\\.dpk'?\\'" . opascal-mode))
  (jcs-add-auto-mode-alist '("\\.dpr'?\\'" . opascal-mode))
  (jcs-add-auto-mode-alist '("\\.org'?\\'" . org-mode)))

;;; P
(progn
  (jcs-add-auto-mode-alist '("\\.pas'?\\'" . pascal-mode))
  (jcs-add-auto-mode-alist '("\\.pl'?\\'" . perl-mode))
  (jcs-add-auto-mode-alist '("\\.pde'?\\'" . processing-mode))
  (jcs-add-auto-mode-alist '("\\.py'?\\'" . python-mode))
  (jcs-add-auto-mode-alist '("\\.pyc'?\\'" . python-mode)))

;;; R
(progn
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
  (jcs-add-auto-mode-alist '("\\.txt'?\\'" . text-mode)))

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
