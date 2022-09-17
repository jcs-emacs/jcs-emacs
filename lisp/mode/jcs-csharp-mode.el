;;; jcs-csharp-mode.el --- C# Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'csharp-mode)

(defun jcs-csharp-ask-source (sc)
  "Ask the source SC for editing CSharp file."
  (interactive
   (list (completing-read
          "Major source for this CSharp file: "
          '("Default" "Godot C#" "Unity C#"))))
  (pcase sc
    ("Default" (jcs-insert-csharp-template))
    ("Godot C#" (jcs-insert-csharp-godot-template))
    ("Unity C#" (jcs-insert-csharp-unity-template))))

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

(jcs-add-hook 'csharp-mode-hook
  (setq-local docstr-show-type-name nil)

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]cs")
                              'jcs-csharp-ask-source
                              :interactive t)

  (jcs-key-local
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next))

      ((kbd "DEL") . jcs-electric-backspace)

      ([f8]   . jcs-find-corresponding-file)
      ([S-f8] . jcs-find-corresponding-file-other-window))))

(provide 'jcs-csharp-mode)
;;; jcs-csharp-mode.el ends here
