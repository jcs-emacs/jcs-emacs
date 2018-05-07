;; ========================================================================
;; $File: jcs-cc-func.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;---------------------------------------------
;; JenChieh Window Split Setting for Dual Monitor
;;---------------------------------------------
(defun jcs-new-window()
  "Setup dual monitor."
  (interactive)

  ;; open a new frame
  (make-frame-command))

;;---------------------------------------------
;; Create a new frame and
;;---------------------------------------------
;; @param frame: frame we just created.
;;---------------------------------------------
(defun jcs-aftermake-frame-functions-hook (frame)
  "Resetting the new frame just created."
  (interactive)

  (select-frame frame)

  ;; split the winodw after create the new window
  (split-window-horizontally))
(add-hook 'after-make-frame-functions 'jcs-aftermake-frame-functions-hook)
