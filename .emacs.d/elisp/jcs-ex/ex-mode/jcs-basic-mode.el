;; ========================================================================
;; $File: jcs-basic-mode.el $
;; $Date: 2018-02-15 15:38:16 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh BASIC mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'basic-mode)
(defun jcs-basic-mode-hook ()
  "Hook for `basic-mode'."


  (defun jcs-basic-script-format ()
    "Format the given file as a class. - JenChieh BASIC."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-basic-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]bas" buffer-file-name) (jcs-basic-script-format))
        )

  ;; BASIC key bindings
  (define-key basic-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key basic-mode-map "\C-c\C-c" 'kill-ring-save)
  )
(add-hook 'basic-mode-hook 'jcs-basic-mode-hook)

(add-to-list 'auto-mode-alist '("\\.bas\\'" . basic-mode))
