;;; jcs-font.el --- Development related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; -- Font Size: The value is in 1/10pt, so 100 will give you 10pt, etc.
(set-face-attribute 'default nil :height 160)


(use-package use-ttf
  :ensure t
  :config
  ;; List of TTF fonts you want to use in the currnet OS.
  (setq use-ttf-default-ttf-fonts '(;; >> Classic Console <<
                                    "/.emacs.jcs/fonts/clacon.ttf"
                                    ;; >> Ubuntu Mono <<
                                    "/.emacs.jcs/fonts/UbuntuMono-R.ttf"))

  ;; Name of the font we want to use as default.
  ;; This you need to check the font name in the system manually.
  (setq use-ttf-default-ttf-font-name "Ubuntu Mono")

  ;; Use the font by `use-ttf-default-ttf-font-name` variable. This will actually
  ;; set your Emacs to your target font.
  (use-ttf-set-default-font))




(provide 'jcs-font)
;;; jcs-font.el ends here
