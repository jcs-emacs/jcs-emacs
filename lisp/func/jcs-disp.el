;;; jcs-disp.el --- Customize display format  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Settings" )
;;

(defun jcs-mode-line--adjust-pad ()
  "Adjust padding for external packages."
  (let ((delta 0))
    (when vertical-scroll-bar
      (when-let* ((data (window-scroll-bars))
                  (shown (nth 2 data))
                  (width (nth 1 data)))
        (setq delta (+ delta width))))
    delta))

(defun jcs-mode-line-render (left right)
  "Render mode line with LEFT and RIGHT alignment."
  (let* ((len-left (length (format-mode-line left)))
         (len-right (length (format-mode-line right)))
         (available-width (- (window-width) (+ len-left len-right)))
         (available-width (+ available-width (jcs-mode-line--adjust-pad))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(defun jcs-flycheck-lighter (state)
  "Return flycheck information for the given error type STATE."
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (errorp (flycheck-has-current-errors-p state))
         (err (or (cdr (assq state counts)) "?"))
         (running (eq 'running flycheck-last-status-change)))
    (if (or errorp running) (format "•%s" err))))

(setq-default
 frame-title-format
 '((:eval invocation-name)
   " - "
   (:eval user-real-login-name) "@" (:eval system-name) ": "
   (:eval (when (buffer-modified-p) " *"))
   (:eval (if buffer-file-name "%f" "%b")))
 mode-line-format
 '((:eval
    (jcs-mode-line-render
     (quote ("%e "
             mode-line-front-space
             mode-line-buffer-identification " "
             (:eval (moody-tab (concat " " (format-mode-line mode-line-modes))))
             " " (:eval (jcs-vc-project))))
     (quote ((:eval
              (when (and (bound-and-true-p flycheck-mode)
                         (or flycheck-current-errors
                             (eq 'running flycheck-last-status-change)))
                (cl-loop for state in '((error   . "#FB4933")
                                        (warning . "#FABD2F")
                                        (info    . "#83A598"))
                         as lighter = (jcs-flycheck-lighter (car state))
                         when lighter
                         concat (propertize lighter 'face `(:foreground ,(cdr state))))))
             (:eval (jcs-vc-info)) " "
             (:eval (moody-tab " %l : %c " 0 'up)) " %p "
             mode-line-end-spaces))))))

;;
;; (@* "Core" )
;;

(defun jcs--set-mode-line-color (ac-lst inac-lst)
  "Set `mode-line' theme faces with AC-LST and INAC-LST."
  (let ((state (frame-focus-state))
        (ac-0 (nth 0 ac-lst)) (ac-1 (nth 1 ac-lst))
        (ic-0 (nth 0 inac-lst)) (ic-1 (nth 1 inac-lst)))
    (set-face-foreground 'mode-line (if state ac-0 ic-0))
    (set-face-background 'mode-line (if state ac-1 ic-1))
    (set-face-foreground 'mode-line-inactive ic-0)
    (set-face-background 'mode-line-inactive ic-1)))

(defun jcs--set-border-color (color)
  "Set the border color with COLOR."
  (set-face-foreground 'vertical-border color)
  (set-face-foreground 'window-divider color))

(defun jcs--set-mode-line-theme (ml-ac-lst ml-inac-lst bc)
  "Set the mode line theme.
ML-AC-LST : mode-line active list.  ML-INAC-LST : mode-line inactive list.
BC : border color."
  (jcs--set-mode-line-color ml-ac-lst ml-inac-lst)
  (jcs--set-border-color bc))

;;
;; (@* "Themes" )
;;

(defun jcs-gray-mode-line ()
  "Gray mode line."
  (interactive)
  (if (jcs-light-theme-p)
      (jcs--set-mode-line-theme
       '("#1C1C1C" "#E5E5E5") '("#000000" "#D7D7D7") "#161616")
    (jcs--set-mode-line-theme
     '("#D2D2D2" "#4D4D4D") '("#CCCCCC" "#333333") "#D2D2D2")))

(defun jcs-dark-green-mode-line ()
  "Dark green mode line."
  (interactive)
  (if (jcs-light-theme-p)
      (jcs--set-mode-line-theme
       '("#000" "#B7D3D3") '("#000" "#99C2C2") "#B7D3D3")
    (jcs--set-mode-line-theme
     '("#CCCCCC" "#3C6A69") '("#CCCCCC" "#2B4D4D") "#3C6A69")))

(defun jcs-dark-blue-mode-line ()
  "Dark blue mode line."
  (interactive)
  (if (jcs-light-theme-p)
      (jcs--set-mode-line-theme
       '("#000" "#92B9DF") '("#000" "#7AA0C6") "#92B9DF")
    (jcs--set-mode-line-theme
     '("#CCCCCC" "#205386") '("#CCCCCC" "#0C3765") "#246AAF")))

(defun jcs-dark-orange-mode-line ()
  "Dark orange mode line."
  (interactive)
  (if (jcs-light-theme-p)
      (jcs--set-mode-line-theme
       '("#1C1C1C" "#CC6633") '("#000000" "#682B12") "#CC6633")
    (jcs--set-mode-line-theme
     '("#D2D2D2" "#CC6633") '("#CCCCCC" "#A4532A") "#CC6633")))

(defun jcs-red-mode-line ()
  "Red mode line."
  (interactive)
  (if (jcs-light-theme-p)
      (jcs--set-mode-line-theme
       '("#CCCCCC" "#FF0000") '("#CCCCCC" "#6A0101") "#FF0000")
    (jcs--set-mode-line-theme
     '("#CCCCCC" "#FF0000") '("#CCCCCC" "#6A0101") "#FF0000")))

(defun jcs-purple-mode-line ()
  "Purple mode line."
  (interactive)
  (if (jcs-light-theme-p)
      (jcs--set-mode-line-theme
       '("#CCCCCC" "#B100EB") '("#CCCCCC" "#650286") "#B100EB")
    (jcs--set-mode-line-theme
     '("#CCCCCC" "#B100EB") '("#CCCCCC" "#650286") "#B100EB")))

(provide 'jcs-disp)
;;; jcs-disp.el ends here
