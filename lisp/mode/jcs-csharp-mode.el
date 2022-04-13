;;; jcs-csharp-mode.el --- C# Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'csharp-mode)

(defun jcs-csharp-ask-source (sc)
  "Ask the source SC for editing CSharp file."
  (interactive
   (list (completing-read
          "Major source for this CSharp file: " '("Default" "Unity Scripting"))))
  (pcase sc
    ("Default" (jcs-insert-csharp-template))
    ("Unity Scripting" (jcs-insert-csharp-unity-template))))

;;
;; (@* "Templates" )
;;

(defun jcs-insert-csharp-template ()
  "Header for C# header file."
  (jcs--file-header--insert "csharp" "default.txt"))

(defun jcs-insert-csharp-unity-template ()
  "Header for Unity C# header file."
  (jcs--file-header--insert "csharp" "unity.txt"))

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
