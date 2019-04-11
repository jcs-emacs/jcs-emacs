;;; jcs-xml-mode.el --- Shader mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'shader-mode)

(defvar jcs-shader-mode-map nil
  "Keymap for `jcs-shader-mode'")

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
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (shader-mode)

  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")

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



(require 'glsl-mode)
(defun jcs-glsl-mode-hook ()
  "GLSL mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (defun jcs-glsl-script-format ()
    "Format the given file as a GLSL shader file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-glsl-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]frag" buffer-file-name) (jcs-glsl-script-format))
          ((string-match "[.]geom" buffer-file-name) (jcs-glsl-script-format))
          ((string-match "[.]glsl" buffer-file-name) (jcs-glsl-script-format))
          ((string-match "[.]vert" buffer-file-name) (jcs-glsl-script-format))
          ))

  ;; Normal
  (define-key glsl-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key glsl-mode-map "\C-c\C-c" #'kill-ring-save)
  )
(add-hook 'glsl-mode-hook 'jcs-glsl-mode-hook)


(provide 'jcs-shader-mode)
;;; jcs-shader-mode.el ends here
