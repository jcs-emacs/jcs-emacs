;;; jcs-processing-mode.el --- Processing mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'processing-mode)

;;(setq processing-location "/path/to/processing-java")
;;(setq processing-application-dir "/path/to/processing-application-dir")
;;(setq processing-sketchbook-dir "/path/to/processing-sketchbook-dir")

;; NOTE(jenchieh): Usually when you run a processing sketch,
;; the build files are generated in a sub-directory called
;; output in the current sketch directory. It is also possible
;; to set the processing-output-dir to another directory:
;;(setq processing-output-dir "/tmp")

(defun jcs-processing-mode-hook ()
  "Hook for processing mode."

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

  (defun jcs-processing-script-format ()
    "Format the given file as a Processing file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-processing-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]pde" buffer-file-name) (jcs-processing-script-format))
          ))

  ;; jcs processing key binding
  (define-key processing-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key processing-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key processing-mode-map (kbd "DEL") #'jcs-delete-backward-char)

  ;; Comment
  (define-key processing-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key processing-mode-map (kbd "*") #'jcs-c-comment-pair)
  )
(add-hook 'processing-mode-hook 'jcs-processing-mode-hook)

(add-to-list 'auto-mode-alist '("\\.pde'?\\'" . processing-mode))


(provide 'jcs-processing-mode)
;;; jcs-processing-mode.el ends here
