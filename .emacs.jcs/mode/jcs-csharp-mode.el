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

(defun jcs-vs-csharp-comment-prefix-p ()
  "Return non-nil if current line is Visual Studio's style comment prefix."
  (jcs-triple-char-comment-prefix-p "/"))

(defun jcs-vs-csharp-only-vs-comment-prefix-this-line-p ()
  "Return non-nil if only comment this line."
  (save-excursion
    (let (only-comment-this-line)
      (when (jcs-vs-csharp-comment-prefix-p)
        (jcs-goto-first-char-in-line)
        (forward-char 3)
        (when (not (jcs-is-there-char-forward-until-end-of-line-p))
          (setq only-comment-this-line t)))
      only-comment-this-line)))

(defun jcs-vs-csharp-do-doc-string ()
  "Return non-nil if able to insert document string."
  (let ((do-doc-string t))
    (jcs-goto-first-char-in-line)
    (while (not (jcs-is-end-of-line-p))
      (forward-char 1)
      (unless (jcs-current-char-equal-p '(" " "\t" "/"))
        (setq do-doc-string nil)))
    do-doc-string))

;;
;; (@* "Hook" )
;;

(defun jcs-csharp-mode-hook ()
  "Hook for C# mode."

  (setq-local docstr-show-type-name nil)

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]cs")
                              'jcs-csharp-ask-source
                              :interactive t)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  (jcs-bind-key (kbd "DEL") #'jcs-electric-backspace)
  (jcs-bind-key (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (jcs-bind-key (kbd ";") #'jcs-vs-semicolon-key)

  (jcs-bind-key [f8] #'jcs-find-corresponding-file)
  (jcs-bind-key [S-f8] #'jcs-find-corresponding-file-other-window)

  (jcs-bind-key (kbd "#") #'jcs-vs-sharp-key)

  (jcs-bind-key (kbd "M-q") #'jcs-other-window-prev))

(add-hook 'csharp-mode-hook 'jcs-csharp-mode-hook)

(provide 'jcs-csharp-mode)
;;; jcs-csharp-mode.el ends here
