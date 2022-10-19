;;; jcs-csharp-mode.el --- C# Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'csharp-mode)

;;
;; (@* "Settings" )
;;

(setq csharp-codedoc-tag-face 'font-lock-doc-face)

;;
;; (@* "Templates" )
;;

;; Ask the source SC for editing CSharp file.
(file-header-defsrc jcs-csharp-ask-source "Major source for this CSharp file: "
  '("Default" "Godot C#" "Unity C#")
  (pcase index
    (0 (jcs-insert-csharp-template))
    (1 (jcs-insert-csharp-godot-template))
    (2 (jcs-insert-csharp-unity-template))))

(file-header-defins jcs-insert-csharp-template "csharp" "default.txt"
  "Header for C# header file.")

(file-header-defins jcs-insert-csharp-godot-template "csharp" "godot.txt"
  "Header for Godot C# header file.")

(file-header-defins jcs-insert-csharp-unity-template "csharp" "unity.txt"
  "Header for Unity C# header file.")

;;
;; (@* "Hook" )
;;

(add-hook 'csharp-mode-hook #'vs-electric-spacing-mode)

(jcs-add-hook 'csharp-mode-hook
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]cs")
                              'jcs-csharp-ask-source
                              :interactive t))

;;
;; (@* "Extensions" )
;;

(leaf meta-view
  :defer-config
  (jcs-add-hook 'meta-view-after-insert-hook
    ;; Hook runs after meta-view buffer insertion.
    (jcs-prog-mode-hook)
    (setq-local ts-fold-summary-show nil)
    (jcs-save-excursion  ; fold all comments
      (goto-char (point-min))
      (call-interactively #'ts-fold-close)
      (let (continuation)
        (while (not (eobp))
          (forward-line 1)
          (end-of-line)
          (if (jcs-inside-comment-p)
              (unless continuation
                (call-interactively #'ts-fold-close)
                (setq continuation t))
            (setq continuation nil)))))))

(provide 'jcs-csharp-mode)
;;; jcs-csharp-mode.el ends here
