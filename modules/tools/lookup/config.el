;;; tools/lookup/config.el  -*- lexical-binding: t; -*-

(leaf dumb-jump
  :init
  (setq dumb-jump-selector 'completing-read))

(leaf define-it
  :init
  (setq define-it-output-choice (if elenv-graphic-p 'frame 'view)
        define-it-text-scale-level -2))

(leaf preview-it
  :init
  (setq preview-it-render-md t))

;;
;; (@* "Definition" )
;;

(defun jcs-goto-definition ()
  "Move to definition."
  (interactive)
  (cond
   ((and (jcs--lsp-connected-p)
         (not (or (ignore-errors (lsp-goto-type-definition))
                  (ignore-errors (lsp-goto-implementation)))))
    t)
   ((derived-mode-p 'lisp-data-mode)
    (if (ignore-errors (call-interactively #'elisp-def))
        (progn (jcs-recenter-top-bottom 'middle) t)
      (user-error "[INFO] No definition found for current target")))
   ((ignore-errors (meta-view-at-point)))
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

;;
;; (@* "Move Between Word (Wrapper)" )
;;

(defun jcs-backward-word-capital (&optional _)
  "Backward search capital character and set the cursor to the point."
  (interactive "^P")
  (let ((max-pt (save-excursion (vs-edit-backward-word) (1+ (point)))))
    (while (and (not (bobp))
                (not (jcs-current-char-uppercasep))
                (> (point) max-pt))
      (backward-char 1))
    (backward-char 1)))

(defun jcs-forward-word-capital (&optional _)
  "Forward search capital character and set the cursor to the point."
  (interactive "^P")
  (let ((max-pt (save-excursion (vs-edit-forward-word) (point))))
    (forward-char 1)
    (while (and (not (eobp))
                (not (jcs-current-char-uppercasep))
                (< (point) max-pt))
      (forward-char 1))))
