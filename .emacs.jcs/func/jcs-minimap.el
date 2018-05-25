;; ========================================================================
;; $File: jcs-minimap-func.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Minimap.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


;;;###autoload
(defun jcs-toggle-minimap ()
  "Toggle minimap. (sublimity)"
  (interactive)

  (if (get 'jcs-toggle-minimap 'state)
      (progn
        (setq sublimity-map-size 0)
        ;; ATTENTION(jenchieh): Set it to very hight so it
        ;; will never reach the timer error.
        (sublimity-map-set-delay 40000000)
        (put 'jcs-toggle-minimap 'state nil))
    (progn
      (setq sublimity-map-size 10)
      ;; NOTE: Set it to nil cost too many performanc...
      (sublimity-map-set-delay 0)
      (put 'jcs-toggle-minimap 'state t)))
  )
