;;; jcs-vs-func.el --- Visual Studio function related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; DESCRIPTION(jenchieh): For function that simulate the Visual Studio
;; IDE's action.

;;;###autoload
(defun jcs-vs-front-curly-bracket-key ()
  "For programming language that need curly bracket."
  (interactive)
  (if (or (jcs-is-current-point-face "font-lock-string-face")
          (jcs-is-inside-comment-block-p))
      (insert "{")
    (progn
      (cond ((and (jcs-first-forward-char-in-line-p "}")
                  (not (jcs-first-backward-char-in-line-p "{")))
             (progn
               (insert "{")))
            (t
             (progn
               (let ((pretty-it nil))
                 (save-excursion
                   ;; Check if right infront of the `{' open curly bracket.
                   (unless (jcs-current-char-equal-p "{")
                     (backward-char 1)
                     (when (jcs-first-backward-char-in-line-p "{")
                       (setq pretty-it t))))

                 (insert "{ }")
                 (backward-char 1)

                 (when pretty-it
                   (save-excursion
                     (forward-char 2)
                     (when (and (not (jcs-is-beginning-of-line-p))
                                (jcs-current-char-equal-p "}"))
                       (backward-char 1)
                       (insert " ")))))))))))

;;;###autoload
(defun jcs-vs-semicolon-key ()
  "For programming language that use semicolon as the end operator sign."
  (interactive)
  (insert ";")

  (save-excursion
    (forward-char 1)
    (when (and (not (jcs-is-beginning-of-line-p))
               (jcs-current-char-equal-p "}"))
      (backward-char 1)
      (insert " "))))

;;;###autoload
(defun jcs-own-delete-backward-char ()
  "This isn't the VS like key action, is more likely to be users own preferences."
  (interactive)
  (save-excursion
    (when (jcs-current-char-equal-p "{")
      (forward-char 1)
      (when (and (not (jcs-is-beginning-of-line-p))
                 (jcs-current-char-equal-p " "))
        (forward-char 1)
        (when (and (not (jcs-is-beginning-of-line-p))
                   (jcs-current-char-equal-p "}"))
          (backward-delete-char 1)))))

  (backward-delete-char 1)

  (save-excursion
    (when (jcs-current-char-equal-p "{")
      (forward-char 1)
      (when (and (not (jcs-is-beginning-of-line-p))
                 (jcs-current-char-equal-p " "))
        (forward-char 1)
        (when (and (not (jcs-is-beginning-of-line-p))
                   (jcs-current-char-equal-p "}"))
          (backward-char 1)
          (backward-delete-char 1))))))

;;;###autoload
(defun jcs-vs-cut-key ()
  "VS like cut key action.
If nothing is selected, we cut the current line. Else we just delete the region."
  (interactive)
  (if (jcs-is-region-selected-p)
      (call-interactively #'kill-region)
    (kill-whole-line)))


(provide 'jcs-vs-func)
;;; jcs-vs-func.el ends here
