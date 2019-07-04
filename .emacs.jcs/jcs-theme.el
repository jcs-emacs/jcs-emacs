;;; jcs-theme.el --- Theme.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-powerline-set-theme-faces (ac-lst inac-lst)
  "Set `powerline' them faces."
  (progn
    (set-face-foreground 'powerline-active0 (nth 0 ac-lst))
    (set-face-background 'powerline-active0 (nth 1 ac-lst))

    (set-face-foreground 'powerline-active1 (nth 2 ac-lst))
    (set-face-background 'powerline-active1 (nth 3 ac-lst))

    (set-face-foreground 'powerline-active2 (nth 4 ac-lst))
    (set-face-background 'powerline-active2 (nth 5 ac-lst)))

  (progn
    (set-face-foreground 'powerline-inactive0 (nth 0 inac-lst))
    (set-face-background 'powerline-inactive0 (nth 1 inac-lst))

    (set-face-foreground 'powerline-inactive1 (nth 2 inac-lst))
    (set-face-background 'powerline-inactive1 (nth 3 inac-lst))

    (set-face-foreground 'powerline-inactive2 (nth 4 inac-lst))
    (set-face-background 'powerline-inactive2 (nth 5 inac-lst)))

  (powerline-reset))

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

  ;; `powerline' font faces.
  (jcs-powerline-set-theme-faces '("#1C1C1C" "#CCCCCC"
                                   "#CCCCCC" "#1C1C1C"
                                   "#CCCCCC" "#333333")
                                 '("#CCCCCC" "#4D4D4D"
                                   "#CCCCCC" "#1C1C1C"
                                   "#CCCCCC" "#333333")))

;;;###autoload
(defun jcs-dark-green-mode-line ()
  "Dark green mode line."
  (interactive)

  ;; set mode line
  (set-face-background 'mode-line "#467E7D")
  (set-face-background 'mode-line-inactive "#2B4D4D")
  ;; set the vertical border
  (set-face-background 'vertical-border "#467E7D")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))

  ;; `powerline' font faces.
  (jcs-powerline-set-theme-faces '("#1C1C1C" "#529191"
                                   "#CCCCCC" "#1C2E2D"
                                   "#CCCCCC" "#294645")
                                 '("#CCCCCC" "#2B4D4D"
                                   "#CCCCCC" "#1C2E2D"
                                   "#CCCCCC" "#294645")))

;;;###autoload
(defun jcs-dark-blue-mode-line ()
  "Dark blue mode line."
  (interactive)

  ;; set the 'vertical border'
  (set-face-background 'vertical-border "#246AAF")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))

  ;; set 'mode line'
  (set-face-background 'mode-line "#246AAF")
  (set-face-background 'mode-line-inactive "#0E2944")

  ;; `powerline' font faces.
  (jcs-powerline-set-theme-faces '("#1C1C1C" "#246AAF"
                                   "#CCCCCC" "#091A2B"
                                   "#CCCCCC" "#0E2944")
                                 '("#CCCCCC" "#14375B"
                                   "#CCCCCC" "#091A2B"
                                   "#CCCCCC" "#0E2944")))

;;;###autoload
(defun jcs-dark-orange-mode-line ()
  "Dark orange mode line."
  (interactive)

  ;; set mode line
  (set-face-background 'mode-line "#FF6C32")
  (set-face-background 'mode-line-inactive "#682B12")
  ;; set the vertical border
  (set-face-background 'vertical-border "#FF6C32")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))

  ;; `powerline' font faces.
  (jcs-powerline-set-theme-faces '("#1C1C1C" "#FF6C32"
                                   "#CCCCCC" "#682B12"
                                   "#CCCCCC" "#9A431F")
                                 '("#CCCCCC" "#9A431F"
                                   "#CCCCCC" "#682B12"
                                   "#CCCCCC" "#883919")))

;;;###autoload
(defun jcs-light-blue-mode-line ()
  "Light blue mode line."
  (interactive)

  ;; set mode line
  (set-face-background 'mode-line "#A3D1FF")
  (set-face-background 'mode-line-inactive "#3685D4")
  ;; set the vertical border
  (set-face-background 'vertical-border "#A3D1FF")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))

  ;; `powerline' font faces.
  (jcs-powerline-set-theme-faces '("#1C1C1C" "#5AA2E9"
                                   "#CCCCCC" "#2C4D6D"
                                   "#CCCCCC" "#3C6894")
                                 '("#CCCCCC" "#3C6894"
                                   "#CCCCCC" "#2C4966"
                                   "#CCCCCC" "#365C82")))

;;;###autoload
(defun jcs-red-mode-line ()
  "Red mode line."
  (interactive)

  ;; set mode line
  (set-face-background 'mode-line "#FF0000")
  (set-face-background 'mode-line-inactive "#6A0101")
  ;; set the vertical border
  (set-face-background 'vertical-border "#FF0000")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))

  ;; `powerline' font faces.
  (jcs-powerline-set-theme-faces '("#1C1C1C" "#FF0000"
                                   "#CCCCCC" "#6A0101"
                                   "#CCCCCC" "#920101")
                                 '("#CCCCCC" "#920101"
                                   "#CCCCCC" "#6A0101"
                                   "#CCCCCC" "#970000")))

;;;###autoload
(defun jcs-purple-mode-line ()
  "Purple mode line."
  (interactive)

  ;; set mode line
  (set-face-background 'mode-line "#B100EB")
  (set-face-background 'mode-line-inactive "#650286")
  ;; set the vertical border
  (set-face-background 'vertical-border "#B100EB")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))

  ;; `powerline' font faces.
  (jcs-powerline-set-theme-faces '("#1C1C1C" "#B100EB"
                                   "#CCCCCC" "#4B0263"
                                   "#CCCCCC" "#69018B")
                                 '("#CCCCCC" "#69018B"
                                   "#CCCCCC" "#4B0263"
                                   "#CCCCCC" "#670188")))


(defun jcs-reset-plugins-base-on-theme ()
  "Reset certain plugins base on the theme."
  (jcs-reset-beacon-color-by-theme)
  (jcs-reset-dashboard-banner-by-theme)
  (jcs-reset-helm-theme-by-theme)
  (jcs-reset-line-number-color-by-theme)
  (jcs-reset-yascroll-color-by-theme)
  (jcs-reset-common-faces-by-theme)
  (when dimmer-mode
    (progn
      ;; Toggle `dimmer-mode'.
      (dimmer-mode -1) (dimmer-mode 1))
    ;; TODO: Weird bug here..
    (dimmer-process-all)))

(defun jcs-set-theme (fgc bgc cc hlc)
  "Setup the theme."
  (jcs-walk-through-all-frames-once
   (lambda ()
     (set-foreground-color fgc)
     (set-background-color bgc)
     (set-cursor-color cc)
     (set-face-background 'hl-line hlc)
     (jcs-reset-plugins-base-on-theme))))

;;;###autoload
(defun jcs-vs-light-theme ()
  "Visual Studio IDE light theme."
  (interactive)
  (jcs-set-theme "#000000"
                 "#FFFFFF"
                 "midnight blue"
                 "#40FF40"))

;;;###autoload
(defun jcs-vs-dark-theme ()
  "Visual Studio IDE dark theme."
  (interactive)
  (jcs-set-theme "#D2D2D2"
                 "#161616"
                 "#40FF40"
                 "midnight blue"))

;;;###autoload
(defun jcs-refresh-theme ()
  "Refresh theme."
  (interactive)
  (if (jcs-is-light-color (face-background 'default))
      (jcs-vs-light-theme)
    (jcs-vs-dark-theme)))

;;;###autoload
(defun jcs-setup-default-theme ()
  "Set default theme color."
  (interactive)
  (jcs-vs-dark-theme))


(provide 'jcs-theme)
;;; jcs-theme.el ends here
