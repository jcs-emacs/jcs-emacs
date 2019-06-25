;;; jcs-vs-func.el --- Visual Studio function related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; DESCRIPTION: For function that simulate the Visual Studio IDE's action.

;;;###autoload
(defun jcs-vs-front-curly-bracket-key ()
  "For programming language that need curly bracket."
  (interactive)
  (if (jcs-inside-comment-or-string-p)
      (insert "{")
    (let ((pretty-it nil)
          (space-infront nil))
      (unless (jcs-current-char-equal-p "{")
        (setq pretty-it t)
        (unless (jcs-current-whitespace-or-tab-p)
          (setq space-infront t)))

      (when space-infront
        (insert " "))

      (insert "{ }")
      (backward-char 1)

      (when pretty-it
        (save-excursion
          (forward-char 2)
          (when (and (not (jcs-is-beginning-of-line-p))
                     (jcs-current-char-equal-p "}"))
            (backward-char 1)
            (insert " ")))))))

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
