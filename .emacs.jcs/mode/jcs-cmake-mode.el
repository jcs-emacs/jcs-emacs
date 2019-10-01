;;; jcs-cmake-mode.el --- CMake mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'cmake-mode)
(require 'cmake-font-lock)

(require 'jcs-make-func)


(defun jcs-cmake-mode-hook ()
  "CMake mode hook."

  ;; File Header
  (jcs-insert-header-if-valid '("CMakeLists.txt")
                              'jcs-insert-cmake-template)

  ;; Normal
  (define-key cmake-mode-map (kbd "<up>") #'jcs-py-indent-up)
  (define-key cmake-mode-map (kbd "<down>") #'jcs-py-indent-down)
  (define-key cmake-mode-map (kbd "RET") #'jcs-makefile-newline)

  ;; tabify save key
  (define-key cmake-mode-map (kbd "C-s") #'jcs-tabify-save-buffer)
  )
(add-hook 'cmake-mode-hook 'jcs-cmake-mode-hook)


(provide 'jcs-cmake-mode)
;;; jcs-cmake-mode.el ends here
