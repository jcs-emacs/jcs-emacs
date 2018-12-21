;; ========================================================================
;; $File: jcs-yaml-func.el $
;; $Date: 2018-12-21 16:21:54 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


;;;###autoload
(defun jcs-yaml-electric-backspace ()
  "Backspace key for editing YAML file."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (call-interactively #'yaml-electric-backspace)))
