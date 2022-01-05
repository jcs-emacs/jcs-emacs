;;; jcs-cmake-mode.el --- CMake mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cmake-mode)
(require 'cmake-font-lock)

(require 'make-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-cmake-template ()
  "CMake file format info."
  (jcs--file-header--insert "cmake" "default.txt"))

;;
;; (@* "Hook" )
;;

(defun jcs-cmake-mode-hook ()
  "CMake mode hook."

  (jcs-company-safe-add-backend 'company-cmake)

  ;; File Header
  (jcs-insert-header-if-valid '("CMakeLists[.]txt")
                              'jcs-insert-cmake-template)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))
  (jcs-bind-key (kbd "RET") #'jcs-makefile-newline))

(add-hook 'cmake-mode-hook 'jcs-cmake-mode-hook)

(provide 'jcs-cmake-mode)
;;; jcs-cmake-mode.el ends here
