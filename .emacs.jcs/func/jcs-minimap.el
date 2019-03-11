;;; jcs-minimap.el --- Minimap function related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


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


(provide 'jcs-minimap)
;;; jcs-minimap.el ends here
