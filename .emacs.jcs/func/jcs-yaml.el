;;; jcs-yaml.el --- YAML related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'yaml-mode)

;;;###autoload
(defun jcs-yaml-electric-backspace ()
  "Backspace key for editing YAML file."
  (interactive)
  (if (use-region-p)
      (jcs-delete-region)
    (call-interactively #'yaml-electric-backspace)))

(provide 'jcs-yaml)
;;; jcs-yaml.el ends here
