;;; jcs-function.el --- Self defines function  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "*Messages*" )
;;

(defun jcs-messages ()
  "Switch to `*Messages*' buffer."
  (interactive)
  (switch-to-buffer (messages-buffer)))

(defun jcs-messages-other-window ()
  "Switch to `*Messages*' buffer."
  (interactive)
  (jcs-switch-to-buffer-other-window (messages-buffer)))

(defun jcs-messages-maybe-kill-this-buffer ()
  "Erase the *Messages* buffer."
  (interactive)
  ;; Message one message to retrieve `*Message*' buffer prepare for next use.
  ;; Or else it some operation might prompt some issue that needed `*Message*'
  ;; buffer to be exists.
  (when (jcs-maybe-kill-this-buffer)
    (message ".") (jcs-messages-erase-buffer)))

(defun jcs-messages-erase-buffer ()
  "Reopen *Messages* buffer."
  (interactive)
  (with-current-buffer (messages-buffer)
    (let (buffer-read-only)
      (erase-buffer)
      (insert (format "Retrieving %s buffer..\n" (buffer-name)))
      (message nil))))  ; clear echo area

;;
;; (@* "*scratch*" )
;;

(defun jcs-scratch-buffer-p ()
  "Return non-nil if current buffer the scratch buffer."
  (equal (current-buffer) (get-scratch-buffer-create)))

(defun jcs-scratch-other-window ()
  "Start a new scratch buffer."
  (interactive)
  (jcs-switch-to-buffer-other-window (get-scratch-buffer-create)))

(defun jcs-new-scratch-buffer ()
  "Start a new scratch buffer."
  (interactive)
  (scratch-buffer)
  (erase-buffer)
  (ignore-errors (insert (substitute-command-keys initial-scratch-message)))
  (goto-char (point-min))
  (lisp-interaction-mode))

(defun jcs-scratch-buffer-refresh ()
  "Refresh scratch buffer."
  (interactive)
  (if (jcs-scratch-buffer-p) (jcs-new-scratch-buffer) (jcs-reopen-this-buffer)))

;;
;; (@* "LSP" )
;;

(defun jcs--lsp-connected-p ()
  "Return non-nil if LSP connected."
  (bound-and-true-p lsp-managed-mode))

(defun jcs--safe-lsp-active ()
  "Safe way to active LSP."
  (unless (jcs--lsp-connected-p) (lsp-deferred)))

;;
;; (@* "Tips" )
;;

(defconst jcs-pop-tooltip-buffer-name "*jcs:pop-tooltip*"
  "Buffer name for tooltip.")

(defun jcs-pop-tooltip--next ()
  "Hide tooltip after first post command."
  (posframe-hide jcs-pop-tooltip-buffer-name)
  (remove-hook 'post-command-hook #'jcs-pop-tooltip--next))

(defun jcs-pop-tooltip--post ()
  "Register for next post command."
  (add-hook 'post-command-hook #'jcs-pop-tooltip--next)
  (remove-hook 'post-command-hook #'jcs-pop-tooltip--post))

(cl-defun jcs-pop-tooltip (string &key point (timeout 300) (height 30))
  "Pop up an tooltip depends on the graphic used.

STRING is the content of the toolip. The location POINT. TIMEOUT for not forever
delay. HEIGHT of the tooltip that will display."
  (jcs-require '(asoc pos-tip popup))
  (let ((bg (asoc-get company-box-doc-frame-parameters 'background-color))
        (fg (asoc-get company-box-doc-frame-parameters 'foreground-color))
        (fringe-width 10))
    (if elenv-graphic-p
        (progn
          (with-current-buffer (get-buffer-create jcs-pop-tooltip-buffer-name)
            (let ((text-scale-mode-step 1.1)) (text-scale-set company-box-doc-text-scale-level)))
          (posframe-show jcs-pop-tooltip-buffer-name :string string :position point
                         :timeout timeout
                         :background-color bg :foreground-color fg
                         :internal-border-width 1
                         :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                         :left-fringe fringe-width :right-fringe fringe-width)
          (add-hook 'post-command-hook #'jcs-pop-tooltip--post))
      (popup-tip string :point point :around t :height height :scroll-bar t :margin t))
    t))

(defun jcs--describe-symbol-string ()
  "Return the describe symbol string."
  (let ((thing (symbol-at-point)))
    (save-window-excursion
      (with-current-buffer (help-buffer)
        (let (buffer-read-only) (erase-buffer))
        (msgu-silent (describe-symbol thing))
        (buffer-string)))))

(defun jcs-tip-describe-it ()
  "Describe symbol at point."
  (let ((desc (jcs--describe-symbol-string)))
    (if (or (string-empty-p desc) (string= (string-trim desc) "[back]"))
        (error "[ERROR] No description at point")
      (jcs-pop-tooltip desc :point (point)))))

(defun jcs-describe-thing-in-popup ()
  "Show current symbol info."
  (interactive)
  (jcs-funcall-fboundp #'company-abort)
  (if (jcs--lsp-connected-p)
      (progn (require 'lsp-ui)
             (or (ignore-errors (call-interactively #'lsp-ui-doc-glance))
                 (ignore-errors (call-interactively #'lsp-ui-doc-show))))
    (cond ((ignore-errors (jcs-tip-describe-it)))
          ((ignore-errors (preview-it)))
          (t (define-it-at-point)))
    ;; In case we are using region, cancel the select region.
    (deactivate-mark)))

(provide 'jcs-function)
;;; jcs-function.el ends here
