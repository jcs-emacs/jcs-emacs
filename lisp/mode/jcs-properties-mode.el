;;; jcs-properties-mode.el --- Properties mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'conf-javaprop-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  (abbrev-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t))

(provide 'jcs-properties-mode)
;;; jcs-properties-mode.el ends here
