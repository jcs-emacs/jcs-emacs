;;; jcs-helm-func.el --- Helm function related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-helm-before-initialize-hook ()
  "Do the helm `M-x' and change theme"
  ;; NOTE: Change theme so we know which mode we are in visually.
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
        (when jcs-helm-ff-other-window
          (setq jcs-helm-ff-other-window nil)
          (helm-ff-run-switch-other-window))
        (helm-maybe-exit-minibuffer))
    (apply orig-fun args)))
(advice-add 'helm-execute-persistent-action :around #'jcs-helm-find-files-navigate-forward)

(defun jcs-helm-find-files-navigate-back (orig-fun &rest args)
  "Advice run around `helm-ff-delete-char-backward' command."
  (if (jcs-current-char-equal-p "/")
      (helm-find-files-up-one-level 1)
    (apply orig-fun args)))
(advice-add 'helm-ff-delete-char-backward :around #'jcs-helm-find-files-navigate-back)


(defvar jcs-helm-ff-other-window nil "Helm find file other window flag.")
(defvar jcs-helm-find-files-active nil "Helm find file flag.")

(defun jcs-helm-find-files-hook ()
  "Hook after `helm-find-files' initialized."
  ;; SEE: `jcs-key.el' file, and `minibuffer-setup-hook'.
  (setq jcs-helm-find-files-active t)
  )
(add-hook 'helm-find-files-after-init-hook 'jcs-helm-find-files-hook)

;;;###autoload
(defun jcs-helm-find-files-other-window ()
  "Find the file with Helm and open another window."
  (interactive)
  (setq jcs-helm-ff-other-window t)
  (helm-find-files nil))

;;;###autoload
(defun jcs-helm-exit-minibuffer ()
  "The command `helm-exit-minibuffer' calls interactively."
  (interactive)
  (helm-exit-minibuffer))

;;;###autoload
(defun jcs-helm-projectile-find-file-other-window ()
  "Find file in project to other window."
  (interactive)
  (let ((record-dd default-directory)
        (found-file nil)
        (starting-window (selected-window)))
    (jcs-other-window-next 1 t)
    (setq default-directory record-dd)
    (setq found-file (helm-projectile-find-file))
    (unless found-file
      (select-window starting-window))))


(provide 'jcs-helm-func)
;;; jcs-helm-func.el ends here
