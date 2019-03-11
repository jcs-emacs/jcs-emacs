;;; jcs-clojure-mode.el --- Clojure mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'clojure-mode)
(defun jcs-clojure-mode-hook ()
  "Clojure mode hook."

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (defun jcs-clojure-format ()
    "Format the given file as a Clojure file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-clojure-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]clj" buffer-file-name) (jcs-clojure-format))
          ))

  ;; jcs Clojure key binding
  (define-key clojure-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key clojure-mode-map "\C-c\C-c" #'kill-ring-save)
  )
(add-hook 'clojure-mode-hook 'jcs-clojure-mode-hook)

(add-to-list 'auto-mode-alist '("\\.clj'?\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs'?\\'" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("\\.cljc'?\\'" . clojurec-mode))


(provide 'jcs-clojure-mode)
;;; jcs-clojure-mode.el ends here
