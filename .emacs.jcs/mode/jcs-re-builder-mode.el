;; ========================================================================
;; $File: jcs-re-builder-mode.el $
;; $Date: 2017-11-27 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh RE-Builder mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 're-builder)

(defun jcs-re-builder-mode-hook ()
  "Mode hook for `RE-Builder-mode'."

  (define-key reb-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key reb-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key reb-mode-map "\ek" #'jcs-reb-maybe-kill-this-buffer)
  )

(add-hook 'reb-mode-hook 'jcs-re-builder-mode-hook)
