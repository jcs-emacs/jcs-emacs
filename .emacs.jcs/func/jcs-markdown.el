;;; jcs-markdown.el --- Markdown related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun jcs-markdown-return-key ()
  "Return key for Markdown mode."
  (interactive)
  (let (did-ret-key close-tag-found)
    (when (and (jcs-first-forward-char-in-line-p "<")
               (jcs-first-backward-char-in-line-p ">"))
      ;; Check closing tag.
      (save-excursion
        (jcs-move-to-forward-a-char "<")
        (forward-char 1)
        (setq close-tag-found (jcs-current-char-equal-p "/")))

      (when close-tag-found
        (newline-and-indent)
        (newline-and-indent)
        (jcs-smart-indent-up)
        (setq did-ret-key t)))

    (unless did-ret-key
      (newline)
      (indent-for-tab-command))))

;;
;; (@* "Faces" )
;;

(defun jcs-init-markdown-faces ()
  "Customize face for `markdown-mode'."
  ;; General
  (set-face-attribute 'markdown-markup-face nil
                      :background (face-background 'default)
                      :foreground "#7EA728")
  ;; Code Block
  (set-face-attribute 'markdown-code-face nil
                      :inherit nil
                      :foreground (face-foreground 'default)
                      :background "#2B2B2B"
                      :extend t)
  ;; List
  (set-face-attribute 'markdown-list-face nil
                      :foreground "gold3")
  ;; Table
  (set-face-attribute 'markdown-table-face nil
                      :foreground "#87CEFA"
                      :background (face-background 'default))
  ;; Header
  (set-face-attribute 'markdown-header-face nil
                      :foreground "#B5CCEB"
                      :background (face-background 'default))
  (set-face-attribute 'markdown-header-delimiter-face nil
                      :foreground "#B5CCEB"
                      :background (face-background 'default)))

(provide 'jcs-markdown)
;;; jcs-markdown.el ends here
