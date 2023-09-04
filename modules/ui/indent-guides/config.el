;;; ui/indent-guides/config.el  -*- lexical-binding: t; -*-

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-suppress-auto-error t))

;;
;; (@* "Faces" )
;;

(defun jcs-highlight-indent-guides--theme (theme)
  "Update theme for `highlight-indent-guides'."
  (pcase theme
    (`vs-dark
     (setq highlight-indent-guides-auto-character-face-perc 150
           highlight-indent-guides-auto-top-character-face-perc 250
           highlight-indent-guides-auto-stack-character-face-perc 200))
    (`vs-light
     (setq highlight-indent-guides-auto-character-face-perc 10
           highlight-indent-guides-auto-top-character-face-perc 30
           highlight-indent-guides-auto-stack-character-face-perc 20))))

(jcs-theme-call #'jcs-highlight-indent-guides--theme)
(add-hook 'jcs-after-load-theme-hook #'jcs-highlight-indent-guides--theme)
