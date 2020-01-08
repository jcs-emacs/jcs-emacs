;;; jcs-web-func.el --- Web Development related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;----------------------------------------------------------------------------
;; Truncate lines depends on the Face.
;;----------------------------------------------------------------------------

(defvar jcs-web-auto-truncate-lines nil
  "Toggle between using automatically truncate lines or not.")

(defun jcs-web-truncate-lines-by-face ()
  "Enable/Disable the truncate lines mode depends on the face cursor currently on."
  (require 'auto-rename-tag)
  (save-excursion
    (when (and (not (jcs-current-char-string-match-p "[ \t\n]"))
               ;; NOTE: To avoid empty line navigation.
               (not (jcs-is-beginning-of-line-p)))
      (if (and (jcs-is-default-face-p)
               (not (jcs-current-char-string-match-p "[><]"))
               (not (auto-rename-tag-inside-tag)))
          (jcs-disable-truncate-lines)
        (jcs-enable-truncate-lines)))))

;;;###autoload
(defun jcs-web-enable-auto-truncate-lines ()
  "Enable auto trancate lines effect."
  (interactive)
  (setq jcs-web-auto-truncate-lines t)
  (message "Enable auto truncate lines."))

;;;###autoload
(defun jcs-web-disable-auto-truncate-lines ()
  "Disable auto trancate lines effect."
  (interactive)
  (setq jcs-web-auto-truncate-lines nil)
  (message "Disable auto truncate lines."))

;;;###autoload
(defun jcs-web-toggle-auto-truncate-lines ()
  "Toggle `jcs-web-auto-truncate-lines' variables."
  (interactive)
  (if jcs-web-auto-truncate-lines
      (jcs-web-disable-auto-truncate-lines)
    (jcs-web-enable-auto-truncate-lines)))

;;;###autoload
(defun jcs-web-keep-auto-truncate-lines ()
  "Keep the same trigger for auto truncate mode.
Is the opposite of `jcs-web-toggle-auto-truncate-lines'."
  (interactive)
  (if jcs-web-auto-truncate-lines
      (jcs-web-enable-auto-truncate-lines)
    (jcs-web-disable-auto-truncate-lines)))

;;----------------------------------------------------------------------------
;; Deletion

;;;###autoload
(defun jcs-web-kill-whole-line ()
  "Kill whole line in web-mode."
  (interactive)
  (jcs-kill-whole-line))

;;;###autoload
(defun jcs-web-kill-ring-save ()
  "Kill ring save in web-mode."
  (interactive)
  (kill-ring-save (region-beginning) (region-end)))

;;;###autoload
(defun jcs-web-yank ()
  "Yank in web-mode."
  (interactive)
  ;; NOTE: No idea why, yank function just need to get wrap by
  ;; another function...

  (jcs-delete-region)

  ;; then paste it.
  (jcs-smart-yank))

;;;###autoload
(defun jcs-web-backward-delete-word ()
  "Web backward delete the word, fit PHP variable naming."
  (interactive)
  (backward-delete-char 1)
  (when (and (not (jcs-current-whitespace-or-tab-p))
             (not (jcs-current-char-equal-p "$"))
             (jcs-current-char-a-wordp))
    (jcs-web-backward-delete-word)))

;;;###autoload
(defun jcs-web-backward-delete-word-capital ()
  "Web backward delete word capital, fit PHP variable naming."
  (interactive)
  (backward-delete-char 1)
  (when (and (not (jcs-current-whitespace-or-tab-p))
             (not (jcs-current-char-equal-p "$"))
             (not (jcs-current-char-uppercasep))
             (jcs-current-char-a-wordp))
    (jcs-web-backward-delete-word-capital))

  (when (and (jcs-current-char-uppercasep)
             (not (jcs-current-char-equal-p "$")))
    (backward-delete-char 1)))

;;----------------------------------------------------------------------------
;; Indentation

;;;###autoload
(defun jcs-web-return-key ()
  "Return key for Web mode."
  (interactive)
  (let ((did-ret-key nil)
        (close-tag-found nil))
    (when (and (jcs-first-forward-char-in-line-p "<")
               (jcs-first-backward-char-in-line-p ">"))
      ;; Check closing tag.
      (save-excursion
        (jcs-move-to-forward-a-char "<")
        (forward-char 1)
        (setq close-tag-found (jcs-current-char-equal-p "/")))

      (when close-tag-found
        (newline-and-indent)
        (newline-and-indent)
        (jcs-smart-indent-up)
        (setq did-ret-key t)))

    (unless did-ret-key
      (call-interactively #'jcs-smart-context-line-break))))

;;;###autoload
(defun jcs-web-vs-opening-curly-bracket-key ()
  "Web mode front curly bracket key."
  (interactive)
  (if (or (jcs-is-current-point-face "web-mode-script-face")
          (jcs-is-current-point-face "web-mode-block-face")
          (jcs-is-current-point-face "web-mode-style-face"))
      (call-interactively #'jcs-vs-opening-curly-bracket-key)
    (insert "{}")
    (backward-char 1)))

;;----------------------------------------------------------------------------
;; Impatient Mode

;;;###autoload
(defun jcs-httpd-start ()
  "Active real time editing with default port (`impatient-mode')."
  (interactive)
  (require 'impatient-mode)
  (call-interactively 'httpd-start)
  (message "Active real time editing with port: %d" httpd-port))

;;;###autoload
(defun jcs-httpd-stop ()
  "Close real time editing with default port. (`impatient-mode')"
  (interactive)
  (require 'impatient-mode)
  (call-interactively 'httpd-stop)
  (message "Close real time editing with port: %d" httpd-port))

;;----------------------------------------------------------------------------
;; Other

;;;###autoload
(defun jcs-toggle-web-mode-offsetless-elements ()
  "Toggle between indent with html tag or not to."
  (interactive)
  (if (get 'jcs-toggle-web-mode-offsetless-elements 'state)
      (progn
        (dolist (tmp-element jcs-web-mode-offsetless-elements-toggle)
          (push tmp-element web-mode-offsetless-elements))
        (put 'jcs-toggle-web-mode-offsetless-elements 'state nil))
    (dolist (tmp-element jcs-web-mode-offsetless-elements-toggle)
      (setq web-mode-offsetless-elements (remove tmp-element web-mode-offsetless-elements)))
    (put 'jcs-toggle-web-mode-offsetless-elements 'state t)))

;;;###autoload
(defun jcs-emmet-expand-line ()
  "Wrapper of `emmet-expand-line' function."
  (interactive)
  (if (jcs-is-current-point-face "link")
      (call-interactively #'goto-address-at-point)
    (unless (call-interactively #'emmet-expand-line)
      (jcs-ctrl-return-key))))

(defun jcs-init-web-faces ()
  "Initialize Web mode faces highlihgting."
  (let ((web-type-comment-missing-modes '(web-mode)))
    (dolist (mode web-type-comment-missing-modes)
      (font-lock-add-keywords
       mode
       '(;; For nomral HTML comment.
         ("\\(<!--[a-zA-Z0-9 \n\t-.<>?,*'`@\"=_(){}:;&^%$#!~]*-->\\)" 1 'font-lock-comment-face t)
         ("\\(@[ \t\n]*{[[:ascii:]]*\\)/\\*[[:ascii:]]*\\*/[[:ascii:]]*}" 1 'jcs-web-mode-block-face t)
         ("@[ \t\n]*{[[:ascii:]]*/\\*[[:ascii:]]*\\*/\\([[:ascii:]]*}\\)" 1 'jcs-web-mode-block-face t)
         ;; For multi-lines comment.
         ("@[ \t\n]*{[[:ascii:]]*\\(/\\*[[:ascii:]]*\\*/\\)[[:ascii:]]*}" 1 'jcs-web-mode-block-comment-face t))
       'end))))


(provide 'jcs-web-func)
;;; jcs-web-func.el ends here
