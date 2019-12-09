;;; jcs-theme.el --- Theme.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs--set-mode-line-color (ac-lst inac-lst)
  "Set `mode-line' them faces with AC-LST and INAC-LST."
  (progn
    (set-face-foreground 'mode-line (nth 0 ac-lst))
    (set-face-background 'mode-line (nth 1 ac-lst)))
  (progn
    (set-face-foreground 'mode-line-inactive (nth 0 inac-lst))
    (set-face-background 'mode-line-inactive (nth 1 inac-lst))))

(defun jcs--set-border-color (color)
  "Set the border color with COLOR."
  (set-face-foreground 'vertical-border color)
  (set-face-foreground 'window-divider color))

(defun jcs-powerline-set-theme-faces (ac-lst inac-lst)
  "Set `powerline' them faces with AC-LST and INAC-LST."
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

(defun jcs--set-mode-line-theme (ml-ac-lst ml-inac-lst bc power-ac-lst power-inac-lst)
  "Set the mode line theme.
ML-AC-LST : mode-line active list.  ML-INAC-LST : mode-line inactive list.
BC : border color.
POWER-AC-LST : powerline active list.  POWER-INAC-LST : powerline inactive list."
  (jcs--set-mode-line-color ml-ac-lst ml-inac-lst)
  (jcs--set-border-color bc)
  (jcs-powerline-set-theme-faces power-ac-lst power-inac-lst))

;;;###autoload
(defun jcs-gray-mode-line ()
  "Gray mode line."
  (interactive)
  (if (jcs-is-light-color (face-background 'default))
      (jcs--set-mode-line-theme
       '("#1C1C1C" "#BFBFBF")
       '("#CCCCCC" "#4D4D4D")
       "#161616"
       '("#1C1C1C" "#D8D8D8"
         "#000000" "#B8B8B8"
         "#1C1C1C" "#C7C7C7")
       '("#1C1C1C" "#CCCCCC"
         "#000000" "#B8B8B8"
         "#1C1C1C" "#C7C7C7"))
    (jcs--set-mode-line-theme
     '("#1C1C1C" "#BFBFBF")
     '("#CCCCCC" "#4D4D4D")
     "#D2D2D2"
     '("#1C1C1C" "#CCCCCC"
       "#CCCCCC" "#1C1C1C"
       "#CCCCCC" "#333333")
     '("#CCCCCC" "#4D4D4D"
       "#CCCCCC" "#1C1C1C"
       "#CCCCCC" "#333333"))))

;;;###autoload
(defun jcs-dark-green-mode-line ()
  "Dark green mode line."
  (interactive)
  (if (jcs-is-light-color (face-background 'default))
      (jcs--set-mode-line-theme
       '("#CCCCCC" "#467E7D")
       '("#CCCCCC" "#2B4D4D")
       "#7ED5D5"
       '("#1C1C1C" "#7ED5D5"
         "#1C1C1C" "#5B928F"
         "#1C1C1C" "#68AFAC")
       '("#1C1C1C" "#75C0C0"
         "#1C1C1C" "#5B928F"
         "#1C1C1C" "#68AFAC"))
    (jcs--set-mode-line-theme
     '("#CCCCCC" "#467E7D")
     '("#CCCCCC" "#2B4D4D")
     "#467E7D"
     '("#1C1C1C" "#529191"
       "#CCCCCC" "#1C2E2D"
       "#CCCCCC" "#294645")
     '("#CCCCCC" "#2B4D4D"
       "#CCCCCC" "#1C2E2D"
       "#CCCCCC" "#294645"))))

;;;###autoload
(defun jcs-dark-blue-mode-line ()
  "Dark blue mode line."
  (interactive)
  (if (jcs-is-light-color (face-background 'default))
      (jcs--set-mode-line-theme
       '("#CCCCCC" "#246AAF")
       '("#CCCCCC" "#0E2944")
       "#2E84D9"
       '("#EDEDED" "#2E84D9"
         "#EDEDED" "#225F9A"
         "#EDEDED" "#2D7AC4")
       '("#EDEDED" "#2C7AC6"
         "#EDEDED" "#225F9A"
         "#EDEDED" "#2D7AC4"))
    (jcs--set-mode-line-theme
     '("#CCCCCC" "#246AAF")
     '("#CCCCCC" "#0E2944")
     "#246AAF"
     '("#1C1C1C" "#246AAF"
       "#CCCCCC" "#091A2B"
       "#CCCCCC" "#0E2944")
     '("#CCCCCC" "#14375B"
       "#CCCCCC" "#091A2B"
       "#CCCCCC" "#0E2944"))))

;;;###autoload
(defun jcs-dark-orange-mode-line ()
  "Dark orange mode line."
  (interactive)
  (if (jcs-is-light-color (face-background 'default))
      (jcs--set-mode-line-theme
       '("#1C1C1C" "#FF6C32")
       '("#CCCCCC" "#682B12")
       "#FF6C32"
       '("#1C1C1C" "#FF6C32"
         "#CCCCCC" "#682B12"
         "#CCCCCC" "#9A431F")
       '("#CCCCCC" "#9A431F"
         "#CCCCCC" "#682B12"
         "#CCCCCC" "#883919"))
    (jcs--set-mode-line-theme
     '("#1C1C1C" "#FF6C32")
     '("#CCCCCC" "#682B12")
     "#FF6C32"
     '("#1C1C1C" "#FF6C32"
       "#CCCCCC" "#682B12"
       "#CCCCCC" "#9A431F")
     '("#CCCCCC" "#9A431F"
       "#CCCCCC" "#682B12"
       "#CCCCCC" "#883919"))))

;;;###autoload
(defun jcs-red-mode-line ()
  "Red mode line."
  (interactive)
  (if (jcs-is-light-color (face-background 'default))
      (jcs--set-mode-line-theme
       '("#CCCCCC" "#FF0000")
       '("#CCCCCC" "#6A0101")
       "#FF0000"
       '("#1C1C1C" "#FF0000"
         "#CCCCCC" "#6A0101"
         "#CCCCCC" "#920101")
       '("#CCCCCC" "#920101"
         "#CCCCCC" "#6A0101"
         "#CCCCCC" "#970000"))
    (jcs--set-mode-line-theme
     '("#CCCCCC" "#FF0000")
     '("#CCCCCC" "#6A0101")
     "#FF0000"
     '("#1C1C1C" "#FF0000"
       "#CCCCCC" "#6A0101"
       "#CCCCCC" "#920101")
     '("#CCCCCC" "#920101"
       "#CCCCCC" "#6A0101"
       "#CCCCCC" "#970000"))))

;;;###autoload
(defun jcs-purple-mode-line ()
  "Purple mode line."
  (interactive)
  (if (jcs-is-light-color (face-background 'default))
      (jcs--set-mode-line-theme
       '("#CCCCCC" "#B100EB")
       '("#CCCCCC" "#650286")
       "#B100EB"
       '("#1C1C1C" "#B100EB"
         "#CCCCCC" "#4B0263"
         "#CCCCCC" "#69018B")
       '("#CCCCCC" "#69018B"
         "#CCCCCC" "#4B0263"
         "#CCCCCC" "#670188"))
    (jcs--set-mode-line-theme
     '("#CCCCCC" "#B100EB")
     '("#CCCCCC" "#650286")
     "#B100EB"
     '("#1C1C1C" "#B100EB"
       "#CCCCCC" "#4B0263"
       "#CCCCCC" "#69018B")
     '("#CCCCCC" "#69018B"
       "#CCCCCC" "#4B0263"
       "#CCCCCC" "#670188"))))

(defun jcs-reset-plugins-base-on-theme ()
  "Reset certain plugins base on the theme."
  (jcs-reset-ahs-by-theme)
  (jcs-reset-dashboard-banner-by-theme)
  (jcs-reset-helm-theme-by-theme)
  (jcs-reset-yascroll-color-by-theme)
  (unless (display-graphic-p) (jcs-reset-common-faces-by-theme))
  (jcs-reload-active-mode)
  (jcs-re-enable-mode-if-was-enabled 'highlight-indent-guides-mode)
  (when (jcs-re-enable-mode-if-was-enabled 'dimmer-mode)
    ;; TODO: Weird bug here..
    (dimmer-process-all)))

(defun jcs-set-theme (cc hlc)
  "Setup the theme to all frames with CC and HLC."
  (jcs-walk-through-all-frames-once
   (lambda ()
     (set-cursor-color cc)
     (set-face-background 'hl-line hlc)
     (jcs-reset-plugins-base-on-theme))))

;;;###autoload
(defun jcs-vs-light-theme ()
  "Visual Studio IDE light theme."
  (interactive)
  (load-theme 'vs-light t)
  (jcs-set-theme "midnight blue" "#40FF40"))

;;;###autoload
(defun jcs-vs-dark-theme ()
  "Visual Studio IDE dark theme."
  (interactive)
  (load-theme 'vs-dark t)
  (jcs-set-theme "#40FF40" "midnight blue"))

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

;;----------------------------------------------------------------------------
;; Load

(defun jcs--load-theme--advice-after (&rest _)
  "Advice execute after `load-theme' function."
  (jcs-set-font-size jcs-default-font-size)
  (jcs-walk-through-all-frames-once
   (lambda ()
     (jcs-reset-plugins-base-on-theme))))
(advice-add 'enable-theme :after #'jcs--load-theme--advice-after)


(provide 'jcs-theme)
;;; jcs-theme.el ends here
