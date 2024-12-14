;;; emacs/image/config.el  -*- lexical-binding: t; -*-

(use-package image-mode
  :bind ( :map image-mode-map
          ("C-r" . image-rotate)
          ("C-0" . (lambda () (interactive)
                     (message "Maximize image is not supported")))
          ("C-=" . image-increase-size)
          ("C--" . image-decrease-size)
          ("C-+" . image-flip-horizontally)
          ("C-_" . image-flip-vertically)))
