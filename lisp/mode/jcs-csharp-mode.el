;;; jcs-csharp-mode.el --- C# Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'csharp-mode)

;; Ask the source SC for editing CSharp file.
(file-header-defsrc jcs-csharp-ask-source "Major source for this CSharp file: "
  '("Default" "Godot C#" "Unity C#")
  (pcase index
    (0 (jcs-insert-csharp-template))
    (1 (jcs-insert-csharp-godot-template))
    (2 (jcs-insert-csharp-unity-template))))

;;
;; (@* "Templates" )
;;

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
                              :interactive t)

  (jcs-key-local
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next))

      ((kbd "DEL") . jcs-electric-backspace))))

(provide 'jcs-csharp-mode)
;;; jcs-csharp-mode.el ends here
