;; ========================================================================
;; $File: jcs-before-init.el $
;; $Date: 2017-07-30 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;================================================
;; Reset all the settings before starting initialize
;; the jcs-package.
;;================================================

;;; Needed packages.

;;; unbind the key
(global-unset-key "\C-k")
(global-unset-key "\C-f")
(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-w"))
