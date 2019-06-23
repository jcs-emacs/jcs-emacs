;;; jcs-processing-mode.el --- Processing mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'processing-mode)


;;(setq processing-location "/path/to/processing-java")
;;(setq processing-application-dir "/path/to/processing-application-dir")
;;(setq processing-sketchbook-dir "/path/to/processing-sketchbook-dir")

;; NOTE: Usually when you run a processing sketch,
;; the build files are generated in a sub-directory called
;; output in the current sketch directory. It is also possible
;; to set the processing-output-dir to another directory:
;;(setq processing-output-dir "/tmp")


(defun jcs-processing-mode-hook ()
  "Hook for processing mode."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]pde" buffer-file-name)
           (jcs-insert-header-if-empty 'jcs-insert-processing-template))
          ))

  ;; Normal
  (define-key processing-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key processing-mode-map (kbd "C-c C-c") #'kill-ring-save)

  (define-key processing-mode-map (kbd "DEL") #'jcs-electric-backspace)

  ;; Comment
  (define-key processing-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key processing-mode-map (kbd "*") #'jcs-c-comment-pair)
  )
(add-hook 'processing-mode-hook 'jcs-processing-mode-hook)


(provide 'jcs-processing-mode)
;;; jcs-processing-mode.el ends here
