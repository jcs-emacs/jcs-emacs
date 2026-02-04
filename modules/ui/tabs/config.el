;;; ui/tabs/config.el  -*- lexical-binding: t; -*-

(use-package centaur-tabs
  :init
  (setq centaur-tabs-style "wave"
        centaur-tabs-set-bar 'under
        centaur-tabs-set-icons t
        centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-icons-prefix ""
        centaur-tabs-icon-scale-factor 0.8
        centaur-tabs-icon-v-adjust 0.01
        centaur-tabs-set-modified-marker t
        centaur-tabs-enable-ido-completion nil
        centaur-tabs-buffer-groups-function #'jcs-tabs-buffer-groups
        centaur-tabs-hide-predicate #'elenv-frame-util-p
        centaur-tabs-hide-tab-function #'centaur-tabs-hide-tab
        centaur-tabs-excluded-prefixes `(" *which")
        centaur-tabs-show-count t
        centaur-tabs-count-format " %d/%d"
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-down-tab-text " ▾ "
        centaur-tabs-backward-tab-text "⏴"
        centaur-tabs-forward-tab-text "⏵"
        ;; Scrolling (with the mouse wheel) past the end of the tab list
        ;; replaces the tab list with that of another Doom workspace. This
        ;; prevents that.
        centaur-tabs-cycle-scope 'tabs)
  :config
  (advice-add 'centaur-tabs-buffer-track-killed :override #'ignore))

;;
;;; Buffer Groups

(defvar jcs-tabs-line--group-cache nil
  "Cache for buffer groups.")

(jcs-add-hook '( centaur-tabs-mode-hook)
  (setq jcs-tabs-line--group-cache (make-hash-table :test #'equal)))

(defun jcs-tabs-clear-dead-buffers ()
  "Remove all dead buffers from group cache."
  (ht-map (lambda (buffer _)
            (unless (buffer-live-p buffer)
              (ht-remove jcs-tabs-line--group-cache buffer)))
          jcs-tabs-line--group-cache))

(defun jcs-tabs-buffer-groups ()
  "Group tabs with cache."
  (let* ((name (buffer-name))
         (buffer (current-buffer))
         (group (or (ht-get jcs-tabs-line--group-cache buffer)
                    (funcall #'jcs-tabs-custom-buffer-groups))))
    (jcs-tabs-clear-dead-buffers)
    (ht-set jcs-tabs-line--group-cache buffer group)
    `(,group)))

(defun jcs-tabs-custom-buffer-groups ()
  "Group tabs."
  (let ((name   (buffer-name))
        (buffer (current-buffer)))
    (cond
;;; Project
     ((when-let* ((project-name (centaur-tabs-project-name)))
        project-name))
;;; A
     ((jcs-member name '("[*]openai" "[*]codegpt" "[*]ChatGPT" "[*]copilot"
                         "[*]google-gemini")
                  'regex)
      "AI")
;;; B
     ((or (memq major-mode '( Buffer-menu-mode))
          (derived-mode-p 'dired-mode))
      "Buffer")
     ((jcs-member name '("[*]eww"
                         "[*]elfeed-")
                  'regex)
      "Browser")
;;; C
     ((jcs-member name '("[*]Completions[*]"
                         "[*]company")
                  'regex)
      "Completion")
     ((jcs-member name '("[*]Compile-Log[*]" "[*]Native-compile-Log[*]"
                         "[*]Async-native-compile-Log[*]")
                  'regex)
      "Compile")
     ((jcs-member name '("[*]Flycheck" "[*]Flymake") 'regex)
      "Checker")
     ((jcs-member name '("[*]jcs") 'regex)
      "Config")
;;; D
     ((memq major-mode '( sqlite-mode))
      "Database")
     ((jcs-member name '("[*]Backtrace[*]"
                         "[*]edebug" "[*]dap-" "[*]debug-")
                  'regex)
      "Debugging")
;;; E
     ((memq major-mode '( message-mode
                          mu4e-compose-mode))
      "Email")
;;; H
     ((or (derived-mode-p 'help-mode)
          (jcs-member name '("[*]Bug Help[*]"
                             "[*]helpful" "[*]suggest[*]")
                      'regex))
      "Debugging")
;;; L
     ((jcs-member name
                  '("[*]Warnings[*]")
                  'regex)
      "Log")
     ((jcs-member name
                  '("[*]lsp-" "-lsp[*]" "[*]LSP[ ]+" "[*]eglot"
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
                    "[*]zig-")
                  'regex)
      "LSP")
;;; M
     ((memq major-mode '( message-mode mu4e-compose-mode))
      "Mail")
     ((memq major-mode '( image-mode))
      "Media")
     ((jcs-member name '("[*]emp")
                  'regex)
      "Music")
;;; N
     ((jcs-member name '("[*]httpd[*]"
                         "[*]HTTP Response[*]"
                         "[*]Ping")
                  'regex)
      "Network")
;;; O
     ((or (derived-mode-p 'compilation-mode 'comint-mode)
          (jcs-member name '("*execrun" "*quickrun") 'regex))
      "Output")
     ((or (memq major-mode '( org-mode org-agenda-mode diary-mode))
          (jcs-member name '("[*]org") 'regex))
      "Org")
;;; P
     ((jcs-member name '("[*]CPU-Profiler-Report"
                         "[*]Memory-Profiler-Report"
                         "[*]Memory Report"
                         "*esup")
                  'regex)
      "Profiler")
;;; R
     ((jcs-member name '("*cider" "*sly")
                  'prefix)
      "REPL")
;;; S
     ((or (derived-mode-p 'shell-mode 'eshell-mode)
          (jcs-member name
                      '("[*]Async Shell Command[*]"
                        "[*]shell" "[*]eshell" "bshell<")
                      'regex))
      "Shell")
     ((member name '("*snow*"))
      "Screen Saver")
;;; T
     ((or
       (memq major-mode '( checkdoc-output-mode))
       (jcs-member name
                   '("[*]Checkdoc " "[*]Elint[*]" "[*]Package-Lint[*]"
                     "[*]relint[*]" "[*]indent-lint"
                     "[*]ert[*]"
                     "[*]Test SHA[*]")
                   'regex))
      "Testing")
     ((jcs-member name '("[*]wclock[*]")
                  'regex)
      "Time")
     ((jcs-member name '("[*]tree-sitter"
                         "tree-sitter-tree:")
                  'regex)
      "Tree-sitter")
;;; U
     ((or (memq major-mode '( grep-mode
                              list-environment-mode
                              list-unicode-display-mode))
          (jcs-member name '("[*]Process List[*]"
                             "[*]ASCII[*]"
                             "[*]Free keys[*]"
                             "[*]RE-Builder"
                             "[*]Most used words[*]"
                             "[*]manage-minor-mode"
                             "[*]Local Variables[*]"
                             "[*]Kill Ring[*]"
                             "[*]timer")
                      'regex))
      "Utilities")
;;; V
     ((or (memq major-mode '( diffview-mode))
          (jcs-member name '("[*]vc"
                             "[*]VC-history[*]"
                             "magit[-]*[[:ascii:]]*[:]")
                      'regex))
      "Version Control")
;;; X
     ((jcs-member name '("[*]xref") 'regex)
      "Xref")

;;; (@* "Fallback" )

;;; Hidden
     ((and (featurep 'buffer-menu-filter)
           (diminish-buffer--filter name))
      "Hidden")
;;; Elisp
     ((derived-mode-p 'emacs-lisp-mode) "Elisp")
;;; Emacs
     ((string-equal "*" (substring name 0 1))
      "Emacs")
;;; Default
     (t
      (centaur-tabs-get-group-name buffer)))))
