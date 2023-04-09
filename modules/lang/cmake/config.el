;;; lang/cmake/config.el  -*- lexical-binding: t; -*-

(require 'cmake-font-lock)

(require 'make-mode)

;;
;; (@* "Header" )
;;

(file-header-defsrc jcs-ask-cmake-template "Select Python template: "
  '(("Empty (Default)" . "File with no content in there")
    ("Root"            . "Specify CMakeLists.txt in project root")
    ("Subdirectory"    . "Specify CMakeLists.txt in subdirectory"))
  (pcase index
    (0 (jcs-insert-cmake-template))
    (1 (jcs-insert-cmake-root-template))
    (2 (jcs-insert-cmake-subdirectory-template))))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-cmake-template "cmake" "default.txt"
  "CMake file template, the default.")

(file-header-defins jcs-insert-cmake-root-template "cmake" "root.txt"
  "CMake file template for root directory.")

(file-header-defins jcs-insert-cmake-subdirectory-template "cmake" "subdirectory.txt"
  "CMake file template for subdirectory.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'cmake-mode-hook
  (setq-local indent-tabs-mode t)

  (company-fuzzy-backend-add-before 'company-cmake 'company-dabbrev)

  ;; File Header
  (jcs-insert-header-if-valid '("CMakeLists[.]txt")
                              'jcs-ask-cmake-template
                              :interactive t)

  (jcs-key-local
    `(((kbd "RET") . jcs-makefile-newline))))

;;
;; (@* "Extensions" )
;;

(use-package eldoc-cmake :hook (cmake-mode . eldoc-cmake-enable))
