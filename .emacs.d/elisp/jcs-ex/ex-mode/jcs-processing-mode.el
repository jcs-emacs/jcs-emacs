;; ========================================================================
;; $File: jcs-processing-mode.el $
;; $Date: 2018-03-18 10:26:03 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Processing mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

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

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")

  (defun jcs-processing-script-format ()
    "Format the given file as a class. - JenChieh Processing Script"
    (when (is-current-file-empty-p)
      (jcs-insert-processing-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]pde" buffer-file-name) (jcs-processing-script-format))
        )

  ;; Set Faces.
  (jcs-oop-init-set-face)

  ;; jcs Lua key binding
  (define-key processing-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key processing-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; Comment
  (define-key processing-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key processing-mode-map (kbd "*") 'jcs-c-comment-pair)

  ;; comment block
  (define-key processing-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  )
(add-hook 'processing-mode-hook 'jcs-processing-mode-hook)

(add-to-list 'auto-mode-alist '("\\.pde?\\'" . processing-mode))
