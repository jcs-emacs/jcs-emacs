;;; jcs-helm-func.el --- Helm function related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-helm-before-initialize-hook ()
  "Do the helm `M-x' and change theme"
  ;; NOTE: Change theme so we know which mode
  ;; we are in visually.
  (jcs-dark-blue-mode-line))
(add-hook 'helm-before-initialize-hook 'jcs-helm-before-initialize-hook)


;; TOPIC: How do I make pressing <RET> in helm-find-files open the directory?
;; SOURCE: http://emacs.stackexchange.com/questions/3798/how-do-i-make-pressing-ret-in-helm-find-files-open-the-directory

(defun jcs-helm-find-files-navigate-forward (orig-fun &rest args)
  "Advice run around `helm-execute-persistent-action' command."
  (if (and (equal "Find Files" (assoc-default 'name (helm-get-current-source)))
           (equal args nil)
           (stringp (helm-get-selection))
           (not (file-directory-p (helm-get-selection))))
      (progn
        (jcs-helm-execute-persistent-action)
        (helm-maybe-exit-minibuffer))
    (apply orig-fun args)))
(advice-add 'helm-execute-persistent-action :around #'jcs-helm-find-files-navigate-forward)

(defun jcs-helm-find-files-navigate-back (orig-fun &rest args)
  "Advice run around `helm-ff-delete-char-backward' command."
  (if (jcs-current-char-equal-p "/")
      (helm-find-files-up-one-level 1)
    (apply orig-fun args)))
(advice-add 'helm-ff-delete-char-backward :around #'jcs-helm-find-files-navigate-back)


;;;
;; `helm-find-files-hook'
;;
(defvar jcs-helm-find-files-active nil
  "Helm find file flag.")

(defun jcs-helm-find-files-hook ()
  "Hook after `helm-find-files' initialized."
  ;; SEE: `jcs-key.el' file, and `minibuffer-setup-hook'.
  (setq jcs-helm-find-files-active t)
  )
(add-hook 'helm-find-files-after-init-hook 'jcs-helm-find-files-hook)


;;;###autoload
(defun jcs-helm-gtags-to-def-dec ()
  "Goto the declaration / definition depends on the cursor position."
  (interactive)
  (ignore-errors
    ;; Update TAG file. Default is update only current file, You
    ;; can update all files with C-u prefix.
    (helm-gtags-update-tags)

    ;; goto definition or declaration.
    (helm-gtags-find-tag-from-here))
  (jcs-reload-active-mode))

;;;###autoload
(defun jcs-helm-gtags-to-def-dec-other-window ()
  "Goto the declaration / definition depends on the cursor position,
in other window."
  (interactive)
  (ignore-errors
    ;; Update TAG file. Default is update only current file, You
    ;; can update all files with C-u prefix.
    (helm-gtags-update-tags)

    ;; NOTE: this will make it jump to next window.
    ;; Is stupid, but work.
    (ignore-errors (helm-gtags-find-tag-other-window nil))

    ;; goto definition or declaration.
    (helm-gtags-find-tag-from-here))

  (jcs-reload-active-mode))

;;;###autoload
(defun jcs-helm-find-files ()
  "Find the file with Helm."
  (interactive)
  (put 'jcs-helm-execute-persistent-action 'state nil)
  (helm-find-files nil))

;;;###autoload
(defun jcs-helm-find-files-other-window ()
  "Find the file with Helm and open another window."
  (interactive)
  ;; set the flag, so when next time run 'jcs-helm-execute-
  ;; persistent-action', he will know what to do instead of
  ;; normal 'helm-execute-persistent-action' action.
  (put 'jcs-helm-execute-persistent-action 'state t)

  (helm-find-files nil))

;;;###autoload
(defun jcs-helm-execute-persistent-action ()
  "Rewrap 'helm-execute-presistent-action' function to my
own preferences."
  (interactive)
  (when (get 'jcs-helm-execute-persistent-action 'state)
    ;; switch the buffer to another window
    (helm-ff-run-switch-other-window)
    (put 'jcs-helm-execute-persistent-action 'state nil)))

;;;###autoload
(defun jcs-helm-projectile-find-file-other-window ()
  "Find file in project to other window."
  (interactive)
  (let ((record-dd default-directory))
    (jcs-other-window-next 1 t)
    (setq default-directory record-dd)
    (call-interactively #'helm-projectile-find-file)))


(provide 'jcs-helm-func)
;;; jcs-helm-func.el ends here
