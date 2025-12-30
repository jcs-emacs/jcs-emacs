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

(defvar jcs-tabs-line--group-cache (make-hash-table :test #'equal)
  "Cache for buffer groups.")

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
     ((memq major-mode '( Buffer-menu-mode))
      "Buffer")
;;; C
     ((jcs-member name '("[*]Completions[*]"
                         "[*]company")
                  'regex)
      "Completion")
     ((jcs-member name '("[*]Flycheck" "[*]Flymake") 'regex)
      "Checker")
     ((jcs-member name '("*cider") 'prefix)
      "Cider")
;;; D
     ((jcs-member name '("[*]Backtrace[*]"
                         "[*]edebug" "[*]dap-" "[*]debug-")
                  'regex)
      "Debugger")
     ((memq major-mode '( diffview-mode))
      "Diff")
     ((derived-mode-p 'dired-mode)
      "Dired")
;;; L
     ((jcs-member name
                  '("[*]lsp-" "[*]LSP[ ]+" "[*]eglot"
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
     ((memq major-mode '( image-mode)) "Media")
;;; N
     ((jcs-member name '("[*]httpd[*]"
                         "[*]HTTP Response[*]")
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
                         "*esup")
                  'regex)
      "Profiler")
;;; S
     ((derived-mode-p 'shell-mode 'eshell-mode)
      "Shell")
     ((member name '("*snow*"))
      "Screen Saver")
     ((jcs-member name '("*sly") 'prefix)
      "Sly")
;;; T
     ((jcs-member name '("[*]ert[*]"
                         "[*]indent-lint")
                  'regex)
      "Testing")
     ((jcs-member name '("[*]tree-sitter"
                         "tree-sitter-tree:")
                  'regex)
      "Tree-sitter")
;;; U
     ((memq major-mode '( grep-mode))
      "Utilities")
;;; V
     ((jcs-member name '("[*]vc"
                         "[*]VC-history[*]"
                         "magit[-]*[[:ascii:]]*[:]")
                  'regex)
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
