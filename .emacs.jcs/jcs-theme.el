;;; jcs-theme.el --- Theme.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;###autoload
(defun jcs-gray-mode-line ()
  "Gray mode line."
  (interactive)

  ;; set mode line
  (set-face-background 'mode-line "#BFBFBF")
  (set-face-background 'mode-line-inactive "#4D4D4D")
  ;; set the vertical border
  (set-face-background 'vertical-border "#D2D2D2")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))

  ;; NOTE: `powerline' font faces.
  (set-face-foreground 'powerline-active1 "#CCCCCC")
  (set-face-background 'powerline-active1 "#1C1C1C")

  (set-face-foreground 'powerline-active2 "#CCCCCC")
  (set-face-background 'powerline-active2 "#333333")

  (set-face-foreground 'powerline-inactive1 "#CCCCCC")
  (set-face-background 'powerline-inactive1 "#1C1C1C")

  (set-face-foreground 'powerline-inactive2 "#CCCCCC")
  (set-face-background 'powerline-inactive2 "#333333")

  ;; Update the `powerline' GUI.
  (powerline-reset)
  )

;;;###autoload
(defun jcs-dark-green-mode-line ()
  "Dark Green mode line."
  (interactive)

  ;; set mode line
  (set-face-background 'mode-line "#467E7D")
  (set-face-background 'mode-line-inactive "#2B4D4D")
  ;; set the vertical border
  (set-face-background 'vertical-border "#467E7D")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))

  ;; NOTE: `powerline' font faces.
  (set-face-foreground 'powerline-active1 "#CCCCCC")
  (set-face-background 'powerline-active1 "#223938")

  (set-face-foreground 'powerline-active2 "#222222")
  (set-face-background 'powerline-active2 "#294645")

  (set-face-foreground 'powerline-inactive1 "#CCCCCC")
  (set-face-background 'powerline-inactive1 "#223938")

  (set-face-foreground 'powerline-inactive2 "#222222")
  (set-face-background 'powerline-inactive2 "#294645")

  ;; Update the `powerline' GUI.
  (powerline-reset)
  )

;;;###autoload
(defun jcs-dark-blue-mode-line ()
  "Dark Blue mode line."
  (interactive)

  (set-cursor-color "#40FF40")
  (set-face-background 'hl-line "midnight blue")

  ;; set the 'vertical border'
  (set-face-background 'vertical-border "#246aaf")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))

  ;; set 'mode line'
  (set-face-background 'mode-line "#246aaf")
  (set-face-background 'mode-line-inactive "#0e2944")

  ;; NOTE: `powerline' font faces.
  (set-face-foreground 'powerline-active1 "#CCCCCC")
  (set-face-background 'powerline-active1 "#091A2B")

  (set-face-foreground 'powerline-active2 "#222222")
  (set-face-background 'powerline-active2 "#246aaf")

  (set-face-foreground 'powerline-inactive1 "#CCCCCC")
  (set-face-background 'powerline-inactive1 "#091A2B")

  (set-face-foreground 'powerline-inactive2 "#CCCCCC")
  (set-face-background 'powerline-inactive2 "#0e2944")

  ;; Update the `powerline' GUI.
  (powerline-reset)
  )

;;;###autoload
(defun jcs-dark-orange-mode-line ()
  "Dark Orange mode line."
  (interactive)

  (set-cursor-color "#40FF40")
  (set-face-background 'hl-line "midnight blue")

  ;; set mode line
  (set-face-background 'mode-line "#FF6C32")
  (set-face-background 'mode-line-inactive "#682B12")
  ;; set the vertical border
  (set-face-background 'vertical-border "#FF6C32")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))

  ;; Update the `powerline' GUI.
  (powerline-reset)
  )

;;;###autoload
(defun jcs-light-blue-mode-line ()
  "Dark Orange mode line."
  (interactive)

  (set-cursor-color "#40FF40")
  (set-face-background 'hl-line "midnight blue")

  ;; set mode line
  (set-face-background 'mode-line "#7aeeef")
  (set-face-background 'mode-line-inactive "#448b8c")
  ;; set the vertical border
  (set-face-background 'vertical-border "#87fdff")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))

  ;; NOTE: `powerline' font faces.
  (set-face-foreground 'powerline-active1 "#CCCCCC")
  (set-face-background 'powerline-active1 "#2B4D4D")

  (set-face-foreground 'powerline-active2 "#222222")
  (set-face-background 'powerline-active2 "#448b8c")

  (set-face-foreground 'powerline-inactive1 "#CCCCCC")
  (set-face-background 'powerline-inactive1 "#2B4D4D")

  (set-face-foreground 'powerline-inactive2 "#222222")
  (set-face-background 'powerline-inactive2 "#448b8c")

  ;; Update the `powerline' GUI.
  (powerline-reset)
  )


;;;###autoload
(defun jcs-vs-light-theme ()
  "Visual Studio IDE light theme."
  (interactive)
  (set-foreground-color "#000000")
  (set-background-color "#FFFFFF")
  (set-cursor-color "#40FF40")
  (set-face-background 'hl-line "midnight blue")
  (jcs-reset-dashboard-banner))

;;;###autoload
(defun jcs-vs-dark-theme ()
  "Visual Studio IDE dark theme."
  (interactive)
  (set-foreground-color "#D2D2D2")
  (set-background-color "#161616")
  (set-cursor-color "#40FF40")
  (set-face-background 'hl-line "midnight blue")
  (jcs-reset-dashboard-banner))

;;;###autoload
(defun jcs-setup-default-theme ()
  "Set default Theme Color."
  (interactive)
  (jcs-vs-dark-theme))

;; Set default theme once.
(unless reload-emacs-reloading
  (jcs-setup-default-theme))


(provide 'jcs-theme)
;;; jcs-theme.el ends here
