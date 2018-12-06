;; ========================================================================
;; $File: jcs-shader-mode.el $
;; $Date: 2017-07-17 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Shader mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'shader-mode)

(defvar jcs-shader-mode-map nil "Keymap for `jcs-shader-mode'")
(progn
  (setq jcs-shader-mode-map (make-sparse-keymap))

  ;; comment block
  (define-key jcs-shader-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key jcs-shader-mode-map (kbd "*") #'jcs-c-comment-pair)
  )

;;;
;; TOPIC(jayces): Elisp: How to Create Keymap for Major Mode
;; URL(jayces): http://ergoemacs.org/emacs/elisp_create_major_mode_keymap.html
(define-derived-mode jcs-shader-mode ()
  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  (shader-mode)

  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")


  (defun jcs-shader-format ()
    "Format the given file as a Unity CG Shader script."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-shader-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]shader" buffer-file-name) (jcs-shader-format))
          ))

  ;; actually no need
  (use-local-map jcs-shader-mode-map)
  )
(add-to-list 'auto-mode-alist '("\\.shader?\\'" . jcs-shader-mode))
