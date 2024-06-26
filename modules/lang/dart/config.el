;;; lang/dart/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-dart-template "dart" "default.txt"
  "Template for Dart.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'dart-mode-hook
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]dart")
                              'jcs-insert-dart-template))

;;
;; (@* "Extensions" )
;;

(use-package flycheck-dart :hook (flycheck-mode . flycheck-dart-setup))
