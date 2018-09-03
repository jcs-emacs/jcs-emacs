;; ========================================================================
;; $File: jcs-vs-func.el $
;; $Date: 2018-08-31 22:46:41 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


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
               ;; If right infront of the `{' front bracket, insert space
               ;; to make it prettier.
               (when (jcs-current-char-equal-p "{")
                 (insert " "))

               (insert "{ }")
               (backward-char 1)

               (save-excursion
                 (forward-char 2)
                 (when (and (not (jcs-is-beginning-of-line-p))
                            (jcs-current-char-equal-p "}"))
                   (backward-char 1)
                   (insert " ")))))))))

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
(defun jcs-delete-backward-char ()
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
