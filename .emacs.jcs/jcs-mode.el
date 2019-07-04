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
        (progn
          (jcs-depend-mode)
          (put 'jcs-depend-cross-mode-toggle 'state nil))
      (jcs-cross-mode)
      (put 'jcs-depend-cross-mode-toggle 'state t))))

;;;###autoload
(defun jcs-reload-active-mode ()
  "Reload the active mode.  Note this is opposite logic to the \
toggle mode function."
  (interactive)
  (if (get 'jcs-depend-cross-mode-toggle 'state)
      ;; if state is true keep on cross mode.
      (jcs-cross-mode)
    ;; vice versa, keep on depend mode.
    (jcs-depend-mode)))


;;;###autoload
(defun jcs-helm-do-ag-this-file ()
  "Handle error for `helm-do-ag-this-file' command by switching
to the `jcs-cross-mode' in order to use cross mode search instead
of machine depenedent plugins/packages which is the `jcs-depend-mode'."
  (interactive)
  (unless (minibufferp)
    (unless (ignore-errors (helm-do-ag-this-file))
      (jcs-cross-mode)
      (message "Error: This buffer is not visited file. Switch to cross mode search..")
      (sleep-for jcs-prompt-message-sleep-delay-time)
      (call-interactively #'isearch-forward))))


(defun jcs-insert-header-if-valid (ext-lst insert-func)
  "Insert the header if certain conditions met.
If one of the EXT-LST, we execute INSERT-FUNC then."
  (when (and buffer-file-name
             (not (file-exists-p buffer-file-name))
             (jcs-is-contain-list-string-regexp ext-lst buffer-file-name))
    (jcs-insert-header-if-empty insert-func)))

(defun jcs-insert-header-if-empty (insert-func &optional ci)
  "Execute INSERT-FUNC if empty, CI means `call-interactively'."
  (when (jcs-is-current-file-empty-p)
    (if ci
        (call-interactively insert-func)
      (funcall insert-func))
    (goto-char (point-min))))


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

  )

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

  )

(defun jcs-view-mode-hook()
  "In view mode, read only file."
  (require 'view)

  ;; unset all the key
  (define-key view-mode-map "a" nil)
  (define-key view-mode-map "b" nil)
  (define-key view-mode-map "c" nil)
  (define-key view-mode-map "d" nil)
  (define-key view-mode-map "e" nil)
  (define-key view-mode-map "f" nil)
  (define-key view-mode-map "g" nil)
  (define-key view-mode-map "h" nil)
  (define-key view-mode-map "i" nil)
  (define-key view-mode-map "j" nil)
  (define-key view-mode-map "k" nil)
  (define-key view-mode-map "l" nil)
  (define-key view-mode-map "m" nil)
  (define-key view-mode-map "n" nil)
  (define-key view-mode-map "o" nil)
  (define-key view-mode-map "p" nil)
  (define-key view-mode-map "q" nil)
  (define-key view-mode-map "r" nil)
  (define-key view-mode-map "s" nil)
  (define-key view-mode-map "t" nil)
  (define-key view-mode-map "u" nil)
  (define-key view-mode-map "v" nil)
  (define-key view-mode-map "w" nil)
  (define-key view-mode-map "x" nil)
  (define-key view-mode-map "y" nil)
  (define-key view-mode-map "z" nil)
  (define-key view-mode-map "," nil)
  (define-key view-mode-map "\\" nil)
  (define-key view-mode-map "." nil)
  (define-key view-mode-map "," nil)
  (define-key view-mode-map "/" nil)
  (define-key view-mode-map "'" nil)
  (define-key view-mode-map " " nil)
  (define-key view-mode-map [tab] nil)
  (define-key view-mode-map (kbd "RET") nil)
  (define-key view-mode-map [space] nil)

  ;; just save buffer, don't care about the tab or spaces.
  (define-key view-mode-map (kbd "C-s") #'save-buffer)
  )
(add-hook 'view-mode-hook 'jcs-view-mode-hook)


;;------------------------------------------------------------------------------------------------------
;;; Local Mode & Online Mode
;;------------------------------------------------------------------------------------------------------

;;;###autoload
(defun jcs-depend-mode ()
  "This mode depend on my own machine. More feature and more control of the editor."
  (interactive)
  (put 'jcs-depend-cross-mode-toggle 'state nil)  ; set toggle trigger

  ;; Customize Mode Line
  (jcs-gray-mode-line)

  ;; Unset 'depend' mode key
  ;; NOTE: unset key should be before of set keys
  (global-unset-key (kbd "C-f"))
  (global-unset-key (kbd "C-r"))

  ;; Set 'depend' mode key

  ;; search
  (define-key global-map (kbd "C-f") #'jcs-helm-do-ag-this-file)
  (define-key global-map (kbd "C-x C-f") #'helm-do-ag-project-root)

  (define-key global-map (kbd "C-r p") #'jcs-ag-project-regexp)

  (jcs-global-key-rebind)
  )

;;;###autoload
(defun jcs-cross-mode ()
  "This mode run anywhere will work, usually less powerful then `jcs-depend-mode'."
  (interactive)

  ;; set toggle trigger
  (put 'jcs-depend-cross-mode-toggle 'state t)

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
  (define-key global-map (kbd "C-x C-f") #'isearch-project-forward)

  (jcs-global-key-rebind)
  )


;;------------------------------------------------------------------------------------------------------
;;; Startup Modes
;;------------------------------------------------------------------------------------------------------

;; NOTE: These are modes that will startup immediately, meaning there will
;; be no benefits having in the separated files except the modulation.
;;
;; So just put all the startup modes' configuration here.

;;==============================
;;      Emacs Lisp
;;------------------------

(defun jcs-emacs-lisp-mode-hook ()
  "Emacs Lisp mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (jcs-make-electric-pair-pairs-local '((?\` . ?\')))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]el" buffer-file-name)
           (jcs-insert-header-if-empty 'jcs-insert-emacs-lisp-template))
          ))

  )
(add-hook 'emacs-lisp-mode-hook 'jcs-emacs-lisp-mode-hook)


;;==============================
;;          Lisp
;;------------------------

(defun jcs-lisp-mode-hook ()
  "Lisp mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (jcs-make-electric-pair-pairs-local '((?\` . ?\')))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]lisp" buffer-file-name)
           (jcs-insert-header-if-empty 'jcs-insert-lisp-template))
          ))

  )
(add-hook 'lisp-mode-hook 'jcs-lisp-mode-hook)

;;==============================
;;     Lisp Interaction
;;------------------------

(defun jcs-lisp-interaction-mode-hook ()
  "Lisp Interaction mode hook."
  (define-key lisp-interaction-mode-map (kbd "M-k") #'jcs-scratch-buffer-maybe-kill)
  (define-key lisp-interaction-mode-map (kbd "M-K") #'jcs-scratch-buffer-refresh))
(add-hook 'lisp-interaction-mode-hook 'jcs-lisp-interaction-mode-hook)

;;==============================
;;          Text
;;------------------------

(defun jcs-text-mode-hook ()
  "Text mode hook."
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]txt" buffer-file-name)
           (jcs-insert-header-if-empty 'jcs-insert-text-template))
          ))

  ;; Normal
  (define-key text-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key text-mode-map (kbd "C-c C-c") #'kill-ring-save)

  (define-key text-mode-map (kbd "<up>") #'jcs-previous-line)
  (define-key text-mode-map (kbd "<down>") #'jcs-next-line)
  )
(add-hook 'text-mode-hook 'jcs-text-mode-hook)


;;------------------------------------------------------------------------------------------------------
;;; Modes
;;------------------------------------------------------------------------------------------------------

(with-eval-after-load 'message (require 'jcs-message-mode))
(with-eval-after-load 're-builder (require 'jcs-re-builder-mode))
(with-eval-after-load 'shell (require 'jcs-shell-mode))
(with-eval-after-load 'esh-mode (require 'jcs-esh-mode))

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
  (jcs-add-auto-mode-alist '("\\.dart'?" . dart-mode)))

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

;;; L
(progn
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
  (jcs-add-auto-mode-alist '("\\(/\\|\\`\\)_vimrc" . vimrc-mode)))

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
