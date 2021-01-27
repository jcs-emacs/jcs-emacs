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

;;
;; (@* "Document String" )
;;

;;;###autoload
(defun jcs-csharp-return ()
  "Return key for `csharp-mode'."
  (interactive)
  (unless (jcs-smart-context-line-break)
    (let (;; Check if the next line is the doc string comment line.
          is-next-line-doc-string-comment-line
          ;; When we break a line and there are still some content on the right.
          line-have-content-on-right)
      (save-excursion
        (unless (jcs-current-line-comment-p)
          (setq line-have-content-on-right t)))

      (save-excursion
        (jcs-previous-line)
        (if (jcs-vs-csharp-only-vs-comment-prefix-this-line-p)
            (when line-have-content-on-right
              (setq is-next-line-doc-string-comment-line t))
          (when (jcs-vs-csharp-comment-prefix-p)
            (setq is-next-line-doc-string-comment-line t))))

      ;; If we still not sure to insert docstring comment line yet.
      ;; Then we need to do deeper check.
      (unless is-next-line-doc-string-comment-line
        (let ((prev-line-vs-prefix nil) (next-line-vs-prefix nil))
          (save-excursion
            (jcs-previous-line)
            (when (jcs-vs-csharp-comment-prefix-p)
              (setq prev-line-vs-prefix t)))

          ;; Only when previous have prefix.
          (when prev-line-vs-prefix
            (save-excursion
              (jcs-next-line)
              (when (jcs-vs-csharp-comment-prefix-p)
                (setq next-line-vs-prefix t)))

            (when (and prev-line-vs-prefix next-line-vs-prefix)
              (setq is-next-line-doc-string-comment-line t)))))

      ;; Is doc-string comment line. Insert doc-string comment.
      (when is-next-line-doc-string-comment-line
        (insert "/// ")))
    (indent-for-tab-command)))

(provide 'jcs-csharp)
;;; jcs-csharp.el ends here
