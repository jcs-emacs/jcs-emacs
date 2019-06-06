;;; jcs-yaml-func.el --- Self defines function.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'yaml-mode)

;;;###autoload
(defun jcs-yaml-electric-backspace ()
  "Backspace key for editing YAML file."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (call-interactively #'yaml-electric-backspace)))


(provide 'jcs-yaml-func)
;;; jcs-yaml-func.el ends here
