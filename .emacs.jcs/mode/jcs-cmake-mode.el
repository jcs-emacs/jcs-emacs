;;; jcs-cmake-mode.el --- CMake mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'cmake-mode)
(require 'cmake-font-lock)

(require 'jcs-makefile-func)
(require 'jcs-python-func)


(defun jcs-cmake-format ()
  "Format the given file as a CMakeLists. - JenChieh CMake"
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-cmake-template)))


(defun jcs-cmake-mode-hook ()
  "CMake mode hook."
  (electric-pair-mode nil)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "CMakeLists.txt" buffer-file-name) (jcs-cmake-format))
          ))

  ;; Normal
  (define-key cmake-mode-map (kbd "<up>") #'jcs-py-indent-up)
  (define-key cmake-mode-map (kbd "<down>") #'jcs-py-indent-down)
  (define-key cmake-mode-map (kbd "RET") #'jcs-makefile-newline)

  (define-key cmake-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key cmake-mode-map (kbd "C-c C-c") #'kill-ring-save)

  ;; tabify save key
  (define-key cmake-mode-map "\C-s" #'jcs-tabify-save-buffer)

  ;; Edit
  (define-key cmake-mode-map (kbd "SPC") #'jcs-py-space)
  (define-key cmake-mode-map (kbd "S-SPC") #'jcs-py-real-space)
  (define-key cmake-mode-map (kbd "<backspace>") #'jcs-py-backspace)
  (define-key cmake-mode-map (kbd "S-<backspace>") #'jcs-py-real-backspace)
  )
(add-hook 'cmake-mode-hook 'jcs-cmake-mode-hook)


(provide 'jcs-cmake-mode)
;;; jcs-cmake-mode.el ends here
