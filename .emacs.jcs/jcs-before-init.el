;;; jcs-before-init.el --- Before initialization.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;================================================
;; Reset all the settings before starting initialize
;; the jcs-package.
;;================================================

;;; Needed packages.

;;; unbind the key
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-f"))
(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-w"))


(provide 'jcs-before-init)
;;; jcs-before-init.el ends here
