;;; tools/lookup/config.el  -*- lexical-binding: t; -*-

(use-package dumb-jump
  :init
  (setq dumb-jump-selector 'completing-read))

(use-package define-it
  :init
  (setq define-it-output-choice (if elenv-graphic-p 'frame 'view)
        define-it-text-scale-level -2)

  (msg-clean-add-echo-commands '( define-it)))

(use-package preview-it
  :init
  (setq preview-it-render-md t))

;;
;; (@* "Definition" )
;;

(defun jcs-goto-definition ()
  "Move to definition."
  (interactive)
  (cond
   ;; LSP
   ((and (jcs-lsp-connected-p)
         (not (or (ignore-errors (lsp-find-definition))
                  (ignore-errors (lsp-goto-type-definition))
                  (ignore-errors (lsp-goto-implementation)))))
    t)
   ;; SLY
   ((and (memq major-mode '(lisp-mode))
         (jcs-fboundp-apply #'sly-connected-p))
    (call-interactively #'sly-edit-definition))
   ;; Emacs Lisp
   ((derived-mode-p 'lisp-data-mode)
    (if (or (ignore-errors (call-interactively #'elisp-def))
            (ignore-errors (xref-find-definitions (elenv-2str (symbol-at-point)))))
        (progn (jcs-recenter-top-bottom 'middle) t)
      (user-error "[INFO] No definition found for current target")))
   ;; C#
   ((ignore-errors (meta-view-at-point)))
   ;; Default
   (t (dumb-jump-go-prefer-external))))

(defun jcs-goto-definition-other-window ()
  "Move to definition other window."
  (interactive)
  (let ((meta-view-display-function #'jcs-switch-to-buffer-other-window))
    (jcs--record-window-excursion-apply
     (jcs--record-window-excursion #'jcs-goto-definition))))

(defun jcs-peek-definition ()
  "Peek definition."
  (interactive)
  (require 'scrollable-quick-peek)
  (when-let* ((buf-list (buffer-list))
              (record (jcs--record-window-excursion #'jcs-goto-definition))
              (buf (nth 0 record)) (ln (nth 1 record)))
    (quick-peek--scroll-to-see)
    (quick-peek-set-spacers buf)
    (scrollable-quick-peek-show (with-current-buffer buf (buffer-string)))
    (setq scrollable-quick-peek-scroll-offset (- ln 3))
    (scrollable-quick-peek-scroll-down)
    ;; If does open the new file to peek, kill the buffer afterward.
    (unless (equal buf-list (buffer-list)) (kill-buffer buf))))
