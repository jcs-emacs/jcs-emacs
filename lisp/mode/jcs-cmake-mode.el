;;; jcs-cmake-mode.el --- CMake mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cmake-mode)
(require 'cmake-font-lock)

(require 'make-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-cmake-template "cmake" "default.txt"
  "CMake file format info.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'cmake-mode-hook
  (setq-local indent-tabs-mode t)

  (company-fuzzy-backend-add 'company-cmake)

  ;; File Header
  (jcs-insert-header-if-valid '("CMakeLists[.]txt")
                              'jcs-insert-cmake-template)

  (jcs-key-local
    `(((kbd "RET") . jcs-makefile-newline))))

(provide 'jcs-cmake-mode)
;;; jcs-cmake-mode.el ends here
