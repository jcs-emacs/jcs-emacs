;; ========================================================================
;; $File: jcs-js-mode.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh JavaScript mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'ac-js2)
(setq ac-js2-evaluate-calls t)
;;(add-hook 'js2-mode-hook 'ac-js2-mode)

(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")

(require 'skewer-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(require 'js2-mode)
;; self define javascript mode here!
(defun jcs-js-mode-hook ()

  ;; enable impatient mode for real time editing.
  (impatient-mode t)

  (setq js2-basic-offset 2)
  (setq js2-bounce-indent-p t)

  ;; enable the stuff you want for JavaScript here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (defun jcs-javascript-format()
    "JavaScript template format."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-js-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]js" buffer-file-name) (jcs-javascript-format))
        )

  ;; jcs javascript key binding
  (define-key js2-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key js2-mode-map "\C-c\C-c" #'kill-ring-save)
  (define-key ac-js2-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key js2-mode-map (kbd "DEL") #'jcs-delete-backward-char)
  (define-key js2-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key js2-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; comment block
  (define-key js2-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key js2-mode-map (kbd "*") #'jcs-c-comment-pair)
  )
(add-hook 'js2-mode-hook 'jcs-js-mode-hook)

(add-to-list 'auto-mode-alist '("\\.js'?\\'" . js2-mode))
