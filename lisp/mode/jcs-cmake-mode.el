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

(jcs-add-hook 'cmake-mode-hook
  (setq-local indent-tabs-mode t)

  (jcs-company-safe-add-backend 'company-cmake)

  ;; File Header
  (jcs-insert-header-if-valid '("CMakeLists[.]txt")
                              'jcs-insert-cmake-template)

  (jcs-key-local
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next))
      ((kbd "RET")    . jcs-makefile-newline))))

(provide 'jcs-cmake-mode)
;;; jcs-cmake-mode.el ends here
