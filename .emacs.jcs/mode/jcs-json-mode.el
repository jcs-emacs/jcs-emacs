;; ========================================================================
;; $File: jcs-json-mode.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh JSON mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'json-mode)
;; self define javascript mode here!
(defun jcs-json-mode-hook ()

  (setq js2-basic-offset 2)
  (setq js2-bounce-indent-p t)

  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2)

  ;; enable the stuff you want for JavaScript here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)


  (defun jcs-json-format()
    "Format for json file."
    (when (jcs-is-current-file-empty-p)
      ;; empty, cause json should only take data.
      ;; Comment will be treat as a data too...
      ))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]json" buffer-file-name) (jcs-json-format))
        )

  ;; jcs javascript key binding
  (define-key json-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key json-mode-map "\C-c\C-c" 'kill-ring-save)

  (define-key json-mode-map (kbd "DEL") #'jcs-delete-backward-char)

  ;; comment block
  (define-key json-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key json-mode-map (kbd "*") 'jcs-c-comment-pair)
  )
(add-hook 'json-mode-hook 'jcs-json-mode-hook)

(add-to-list 'auto-mode-alist '("\\.json?\\'" . json-mode))
