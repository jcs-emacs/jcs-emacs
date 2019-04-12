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
        ;; command mode
        (jcs-command-mode)
        (put 'jcs-insert-command-mode-toggle 'state nil))
    (progn
      ;; insert mode
      (jcs-insert-mode)
      (put 'jcs-insert-command-mode-toggle 'state t))))

;;;###autoload
(defun jcs-depend-cross-mode-toggle()
  "Toggle depend/cross mode."
  (interactive)
  ;; NOTE(jenchieh): can only active when the minibuffer is
  ;; not active.
  (when (eq jcs-minibuffer-active nil)
    (if (get 'jcs-depend-cross-mode-toggle 'state)
        (progn
          ;; depend mode
          (jcs-depend-mode)
          (put 'jcs-depend-cross-mode-toggle 'state nil))
      (progn
        ;; cross mode
        (jcs-cross-mode)
        (put 'jcs-depend-cross-mode-toggle 'state t)))))

;;;###autoload
(defun jcs-reload-active-mode ()
  "Reload the active mode.  Note this is opposite logic to the \
toggle mode function."
  (interactive)
  ;; NOTE(jenchieh): can only active when the minibuffer is
  ;; not active.
  (when (eq jcs-minibuffer-active nil)
    (if (get 'jcs-depend-cross-mode-toggle 'state)
        ;; if state is true keep on cross mode.
        (jcs-cross-mode)
      ;; vice versa, keep on depend mode.
      (jcs-depend-mode))))


(defvar jcs-prompt-message-sleep-delay-time 0.4  ;; in seconds
  "Delay for a time for prompting out the message, so the user
can see the error/operation message.")

;;;###autoload
(defun jcs-helm-do-ag-this-file ()
  "Handle error for `helm-do-ag-this-file' command by switching
to the `jcs-cross-mode' in order to use cross mode search instead
of machine depenedent plugins/packages which is the `jcs-depend-mode'."
  (interactive)
  ;; NOTE(jenchieh): can only active when the minibuffer is
  ;; not active.
  (unless jcs-minibuffer-active
    (unless (ignore-errors (or (helm-do-ag-this-file) t))
      (jcs-cross-mode)
      (message "Error: This buffer is not visited file. Switch to cross mode search..")
      (sleep-for jcs-prompt-message-sleep-delay-time)
      (call-interactively 'isearch-forward))))

;;---------------------------------------------
;; Command Mode
;;---------------------------------------------

;;;###autoload
(defun jcs-command-mode()
  "In command mode. - JenChieh"
  (interactive)

  ;; set trigger
  (put 'jcs-insert-command-mode-toggle 'state nil)

  ;; switch to view mode
  ;;(view-mode-enable)

  ;; -----------------------------------------
  ;; Customize Theme
  ;; -----------------------------------------
  (jcs-gray-theme)

  ;; -----------------------------------------
  ;; Unset insert mode key
  ;;
  ;; NOTE(jenchieh): unset key should be
  ;; before of set keys
  ;; -----------------------------------------

  ;; -----------------------------------------
  ;; Set command mode key
  ;; -----------------------------------------

  )


;;---------------------------------------------
;; Insert Mode
;;---------------------------------------------

;;;###autoload
(defun jcs-insert-mode()
  "In insert mode. - JenChieh"
  (interactive)

  ;; set trigger
  (put 'jcs-insert-command-mode-toggle 'state t)

  ;; disable to view mode
  ;;(view-mode-disable)

  ;; -----------------------------------------
  ;; Customize Theme
  ;; -----------------------------------------
  (jcs-dark-green-theme)

  ;; -----------------------------------------
  ;; Unset command mode key
  ;;
  ;; NOTE(jenchieh): unset key should be
  ;; before of set keys
  ;; -----------------------------------------

  ;; -----------------------------------------
  ;; Set insert mode key
  ;; -----------------------------------------

  )

;; Make command mode start at the beginning.
(call-interactively #'jcs-command-mode)


;;------------------------------------------------------------------------------------------------------
;;; View Mode
;;------------------------------------------------------------------------------------------------------

(defun jcs-view-mode-hook()
  "In view mode, read only file."
  (interactive)

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
  (define-key view-mode-map "\C-s" 'save-buffer)
  )
(add-hook 'view-mode-hook 'jcs-view-mode-hook)


;;------------------------------------------------------------------------------------------------------
;;; Local Mode & Online Mode
;;------------------------------------------------------------------------------------------------------

;;;###autoload
(defun jcs-depend-mode ()
  "This mode depend on my own machine. More feature and more \
control of the editor."
  (interactive)

  ;; set toggle trigger
  (put 'jcs-depend-cross-mode-toggle 'state nil)

  ;; -----------------------------------------
  ;; Customize Theme
  ;; -----------------------------------------
  (jcs-gray-theme)

  ;; -----------------------------------------
  ;; Unset 'depend' mode key
  ;;
  ;; NOTE(jenchieh): unset key should be
  ;; before of set keys
  ;; -----------------------------------------
  (global-unset-key "\C-f")
  (global-unset-key "\C-r")

  ;; -----------------------------------------
  ;; Set 'depend' mode key
  ;; -----------------------------------------

  ;; search
  (define-key global-map "\C-f" 'jcs-helm-do-ag-this-file)
  (define-key global-map "\C-x\C-f" 'helm-do-ag-project-root)

  ;; Search
  (define-key global-map "\C-rp" 'jcs-ag-project-regexp)

  (when (functionp 'jcs-global-key-rebind)
    (jcs-global-key-rebind))
  )

;;;###autoload
(defun jcs-cross-mode ()
  "This mode run anywhere will work, usually less powerful then \
'jcs-depend-mode'."
  (interactive)

  ;; set toggle trigger
  (put 'jcs-depend-cross-mode-toggle 'state t)

  ;; -----------------------------------------
  ;; Customize Theme
  ;; -----------------------------------------
  (jcs-dark-green-theme)

  ;; -----------------------------------------
  ;; Unset 'cross' mode key
  ;;
  ;; NOTE(jenchieh): unset key should be
  ;; before of set keys
  ;; -----------------------------------------
  (global-unset-key "\C-f")
  (global-unset-key "\C-r")

  ;; -----------------------------------------
  ;; Set 'cross' mode key
  ;; -----------------------------------------

  ;; search
  (define-key global-map "\C-f" 'isearch-forward)
  (global-unset-key "\C-x\C-f")

  ;; Search
  (global-unset-key "\C-rp")

  (when (functionp 'jcs-global-key-rebind)
    (jcs-global-key-rebind))
  )



;; Modes
(require 'jcs-message-mode)
(with-eval-after-load 're-builder (require 'jcs-re-builder-mode))
(with-eval-after-load 'shell (require 'jcs-shell-mode))
(require 'jcs-txt-mode)

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
(with-eval-after-load 'csharp-mode (require 'jcs-csharp-mode))
(with-eval-after-load 'css-mode (require 'jcs-css-mode))
(with-eval-after-load 'elisp-mode (require 'jcs-elisp-mode))
(with-eval-after-load 'go-mode (require 'jcs-go-mode))
(with-eval-after-load 'haskell-mode (require 'jcs-haskell-mode))
(with-eval-after-load 'haxe-mode (require 'jcs-haxe-mode))
(with-eval-after-load 'ini-mode (require 'jcs-ini-mode))
(with-eval-after-load 'jayces-mode (require 'jcs-jayces-mode))
(with-eval-after-load 'js2-mode (require 'jcs-js-mode))
(with-eval-after-load 'json-mode (require 'jcs-json-mode))
(with-eval-after-load 'lisp-mode (require 'jcs-lisp-mode))
(with-eval-after-load 'lua-mode (require 'jcs-lua-mode))
(with-eval-after-load 'make-mode (require 'jcs-makefile-mode))
(with-eval-after-load 'markdown-mode (require 'jcs-markdown-mode))
(with-eval-after-load 'nasm-mode (require 'jcs-nasm-mode))
(with-eval-after-load 'nxml-mode (require 'jcs-xml-mode))
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
  ;; NOTE(jenchieh): For autotools, autoconf, automake.
  (jcs-add-auto-mode-alist '("\\.ac'?\\'" . cmake-mode))
  (jcs-add-auto-mode-alist '("\\.cbl'?\\'" . cobol-mode))
  (jcs-add-auto-mode-alist '("\\.cs'?\\'" . csharp-mode))
  (jcs-add-auto-mode-alist '("\\.css'?" . css-mode)))

;;; E
(progn
  (jcs-add-auto-mode-alist '("\\.el'?\\'" . emacs-lisp-mode)))

;;; G
(progn
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

  ;; STUDY(jenchieh): haxe-mode is seems to be a void function,
  ;; is weird we need this here.
  (require 'haxe-mode)
  (jcs-add-auto-mode-alist '("\\.hx'?\\'" . haxe-mode)))

;;; I
(progn
  (jcs-add-auto-mode-alist '("\\.properties'?\\'" . ini-mode))
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
  (jcs-add-auto-mode-alist '("\\.org'?\\'" . org-mode))
  (jcs-add-auto-mode-alist '("\\(/\\|\\`\\)README" . org-mode))
  (jcs-add-auto-mode-alist '("\\(/\\|\\`\\)LICENSE" . org-mode))
  (jcs-add-auto-mode-alist '("\\(/\\|\\`\\)bochsrc" . org-mode)))

;;; P
(progn
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
  (jcs-add-auto-mode-alist '("\\.ts'?\\'" . typescript-mode)))

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
