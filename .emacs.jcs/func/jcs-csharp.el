;;; jcs-csharp.el --- C# related  -*- lexical-binding: t -*-
;;; Commentary: When editing the C# related file.
;;; Code:

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
;; (@* "Indentation" )
;;

(defun jcs-csharp-smart-indent-up ()
  "CSharp mode smart indent up."
  (interactive)
  (jcs-smart-indent-up)
  (when (and (jcs-is-end-of-line-p)
             (jcs-current-char-equal-p "/")
             (jcs-vs-csharp-only-vs-comment-prefix-this-line-p))
    (insert " ")))

(defun jcs-csharp-smart-indent-down ()
  "CSharp mode smart indent down."
  (interactive)
  (jcs-smart-indent-down)
  (when (and (jcs-is-end-of-line-p)
             (jcs-current-char-equal-p "/")
             (jcs-vs-csharp-only-vs-comment-prefix-this-line-p))
    (insert " ")))

(provide 'jcs-csharp)
;;; jcs-csharp.el ends here
