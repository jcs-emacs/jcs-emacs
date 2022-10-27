;;; tools/tree-sitter/config.el  -*- lexical-binding: t; -*-

(leaf tree-sitter-langs
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :defer-config
  (defun jcs--tree-sitter-grab-queries ()
    "Download all custom queries to the `tree-sitter-langs' queries folder."
    (require 'find-func)
    (let* ((default-directory (file-name-directory (find-library-name "tree-sitter-langs")))
           (repo "https://github.com/jcs-emacs/tree-sitter-queries")
           (repo-url (shell-quote-argument repo))
           (dirname (file-name-base repo))
           (clone-dir (expand-file-name dirname))
           (clone-queries (expand-file-name "queries" clone-dir))
           (dest-queries (expand-file-name "queries" default-directory))
           lang-dirs)
      (ignore-errors (delete-directory (expand-file-name dirname default-directory) t))
      (when (= 0 (shell-command (format "git clone %s" repo-url)))
        (setq lang-dirs (directory-files clone-queries))
        (pop lang-dirs) (pop lang-dirs)  ; remove . and ..
        (message "Installing custom tree-sitter query...")
        (dolist (lang-dir lang-dirs)
          (message "  - %s" lang-dir)
          (ignore-errors
            (delete-directory (expand-file-name lang-dir clone-queries)))
          (ignore-errors
            (copy-directory (expand-file-name lang-dir clone-queries)
                            (expand-file-name lang-dir dest-queries)
                            nil nil t)))
        (delete-directory clone-dir t)
        (message "Done install custom tree-sitter queries"))))

  (defun jcs--tree-sitter-hl-mode-hook ()
    "Hook for `tree-sitter-hl-mode'."
    (remove-hook 'tree-sitter-hl-mode-hook #'jcs--tree-sitter-hl-mode-hook)
    (jcs--tree-sitter-grab-queries)
    (tree-sitter-hl-mode 1))  ; re-enable it once
  (add-hook 'tree-sitter-hl-mode-hook #'jcs--tree-sitter-hl-mode-hook))

(leaf ts-docstr
  :hook (tree-sitter-after-on-hook . ts-docstr-mode)
  :init
  (setq ts-docstr-key-support t
        ts-docstr-desc-summary ""))
