;;; emacs/buffer-menu/config.el  -*- lexical-binding: t; -*-

(use-package buffer-menu-filter
  :init
  (setq buffer-menu-filter-delay 0.2))

(use-package diminish-buffer
  :init
  (setq diminish-buffer-list
        '("[*]jcs"  ; config wise
          "[*]Echo Area" "[*]Minibuf-"
          "[*]helm" "[*]esup" "[*]quelpa-"
          "[*]easky" "[*]quickrun"
          "[*]Apropos[*]" "[*]Compile-Log[*]"
          "[*]Ibuffer[*]"
          "[*]Bug Help[*]"
          "[*]Warnings[*]"
          "[*]vc" "[*]VC-history[*]"
          "[*]CPU-Profiler-Report" "[*]Memory-Profiler-Report"
          "[*]Process List[*]"
          "[*]Checkdoc " "[*]Elint[*]" "[*]Package-Lint[*]" "[*]relint[*]"
          "[*]Finder[*]"
          "[*]Async Shell Command[*]" "[*]shell" "[*]eshell" "bshell<"
          "[*]envrc"
          "[*]eww" "[*]ESS[*]"
          "[*]Ping"
          "[*]emacs[*]"  ; From `async'
          "[*]sly"
          "[*]cider-" "[*]nrepl-server"
          "[*]timer"
          ;; `Debugger'
          "[*]Backtrace[*]"
          "[*]edebug" "[*]dap-" "[*]debug-"
          ;; `LSP'
          "[*]lsp-" "[*]LSP[ ]+" "[*]eglot"
          "[*][[:ascii:]]*ls[*:-]" "out[*]" "stderr[*]"
          "[*]clang-" "[*]clangd"
          "[*]csharp[*]"
          "[*]cogru"
          "[*]cucumber"
          "[*]dart"
          "[*]ellsp" "[*]elsa"
          "[*]eslint"
          "[*]perlnavigator"
          "[*]lua-"
          "[*]iph[*]"
          "[*]rust-analyzer[*:]"
          "[*]sql"
          "[*]zig-"
          "[*]Coursier log[*]"
          "[*]tcp-server-"
          "[*]Python" "[*]pyright[*]"
          "[*]tree-sitter" "tree-sitter-tree:"
          "[*]Completions[*]" "[*]company"
          "[*]eldoc"
          "[*]editorconfig"
          "[*]prettier"
          "[*]Local Variables[*]"
          "[*]Kill Ring[*]"  ; From `browse-kill-ring'
          "[*]SPEEDBAR"
          "[*]Flycheck" "[*]Flymake"
          "[*]httpd[*]" "[*]HTTP Response[*]"
          "[*]helpful" "[*]suggest[*]"
          "[*]ert[*]" "[*]indent-lint"
          "[*]elfeed-"
          "magit[-]*[[:ascii:]]*[:]"  ; From `magit'
          "[*]openai" "[*]codegpt" "[*]ChatGPT" "[*]copilot" "[*]google-gemini"
          "[*]Most used words[*]"
          "[*]manage-minor-mode"
          "[*]Free keys[*]"
          "[*]Test SHA[*]"
          "[*]RE-Builder"
          "[*]xref" "[*]define-it: tooltip[*]" "[*]preview-it" "[*]gh-md"
          "[*]wclock[*]"
          "[*]Clippy[*]"
          "[*]CMake Temporary[*]"
          "[*]org"
          "[*]ASCII[*]"
          "[*]e2ansi"
          "[*]npm:" "[*]hexo"
          "[*]Flutter"
          "[*]emp"
          "[*]snow[*]")
        diminish-buffer-mode-list '("buffer-menu-mode"
                                    "completion-list-mode"
                                    "compilation-mode" "comint-mode"
                                    "dired-mode"
                                    "grep-mode"
                                    "help-mode" "custom-mode"
                                    "list-environment-mode"
                                    "list-unicode-display-mode"
                                    "shell-mode" "eshell-mode"
                                    "sqlite-mode"
                                    "checkdoc-output-mode"
                                    "proof-splash-mode"
                                    "diffview-mode")))

;;
;; (@* "Hook" )
;;

(require 'buffer-menu-project)
(buffer-menu-filter-mode 1)
(diminish-buffer-mode 1)  ; enable by default
(nerd-icons-buffer-menu-mode 1)

(jcs-add-hook 'Buffer-menu-mode-hook
  (jcs-key-local
    `(((kbd "C-k"))
      ((kbd "M-K")      . buffer-menu-filter-refresh)
      ;; Searching / Filtering
      ((kbd "<escape>") . (lambda () (interactive) (buffer-menu-filter-refresh)
                            (top-level))))))

(jcs-add-hook 'diminish-buffer-mode-hook
  (setq centaur-tabs-groups-hash (make-hash-table :test 'equal)
        centaur-tabs-hide-hash (make-hash-table :test 'equal)))

(use-package nerd-icons-buffer-menu
  :init
  (setq nerd-icons-buffer-menu-icon-scale-factor 0.8
        nerd-icons-buffer-menu-icon-v-adjust 0.1))

;;
;; (@* "Core" )
;;

(defun jcs-buffer-menu ()
  "Enter buffer menu."
  (interactive)
  (if (get-buffer-window diminish-buffer-menu-name)
      (switch-to-buffer diminish-buffer-menu-name)
    (buffer-menu)))

(defun jcs-buffer-menu-other-window ()
  "Enter buffer menu other window."
  (interactive)
  (if (get-buffer-window diminish-buffer-menu-name)
      (switch-to-buffer-other-window diminish-buffer-menu-name)
    (buffer-menu-other-window)))

(defun jcs-buffer-menu-project ()
  "Enter buffer menu for project."
  (interactive)
  (if-let* ((buf-name (buffer-menu-project-buffer-name))
            ((get-buffer-window buf-name)))
      (switch-to-buffer buf-name)
    (buffer-menu-project)))

(defun jcs-buffer-menu-project-other-window ()
  "Enter buffer menu for project other window."
  (interactive)
  (if-let* ((buf-name (buffer-menu-project-buffer-name))
            ((get-buffer-window buf-name)))
      (switch-to-buffer-other-window buf-name)
    (buffer-menu-project-other-window)))

(defun jcs-buffer-menu-p ()
  "Check if current major mode `buffer-menu'."
  (eq major-mode 'Buffer-menu-mode))

(defun jcs-buffer-menu-refresh-buffer ()
  "Update buffer menu buffer."
  (interactive)
  (jcs-when-buffer-window diminish-buffer-menu-name
    (msgu-silent (buffer-menu-filter-refresh-preserve))))
