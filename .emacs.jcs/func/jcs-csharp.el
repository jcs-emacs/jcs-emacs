;;; jcs-csharp.el --- C# related  -*- lexical-binding: t -*-
;;; Commentary: When editing the C# related file.
;;; Code:

;;;###autoload
(defun jcs-csharp-ask-source (sc)
  "Ask the source SC for editing CSharp file."
  (interactive
   (list (completing-read
          "Major source for this CSharp file: " '("Default"
                                                  "Unity Scripting"))))
  (cond ((string= sc "Default") (jcs-insert-csharp-template))
        ((string= sc "Unity Scripting") (jcs-insert-csharp-unity-template))))

(defun jcs-vs-csharp-comment-prefix-p ()
  "Check if current line is a Visual Studio's style comment prefix."
  (jcs-triple-char-comment-prefix-p "/"))

(defun jcs-vs-csharp-only-vs-comment-prefix-this-line-p ()
  "Check if there is only comment in this line and is Visaul Studio \
comment prefix only."
  (save-excursion
    (let ((only-comment-this-line nil))
      (when (jcs-vs-csharp-comment-prefix-p)
        (jcs-goto-first-char-in-line)
        (forward-char 3)
        (when (not (jcs-is-there-char-forward-until-end-of-line-p))
          (setq only-comment-this-line t)))
      only-comment-this-line)))

(defun jcs-vs-csharp-do-doc-string ()
  "Check if should insert the doc string by checking only comment characters \
on the same line."
  (let ((do-doc-string t))
    (jcs-goto-first-char-in-line)
    (while (not (jcs-is-end-of-line-p))
      (forward-char 1)
      (unless (jcs-current-char-equal-p '(" " "\t" "/"))
        ;; return false.
        (setq do-doc-string nil)))
    ;; return true.
    do-doc-string))

;;
;; (@* "Indentation" )
;;

;;;###autoload
(defun jcs-csharp-smart-indent-up ()
  "CSharp mode smart indent up."
  (interactive)
  (jcs-smart-indent-up)
  (when (and (jcs-is-end-of-line-p)
             (jcs-current-char-equal-p "/")
             (jcs-vs-csharp-only-vs-comment-prefix-this-line-p))
    (insert " ")))

;;;###autoload
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
