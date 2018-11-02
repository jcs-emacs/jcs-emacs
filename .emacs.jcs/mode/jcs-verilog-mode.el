;; ========================================================================
;; $File: jcs-verilog-mode.el $
;; $Date: 2017-08-04 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


(require 'verilog-mode)
(defun jcs-verilog-mode-hook ()
  "Verilog mode hook."
  (interactive)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  (defun jcs-verilog-script-format ()
    "Format the given file as a Verilog file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-verilog-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]v" buffer-file-name) (jcs-verilog-script-format))
        )

  )
(add-hook 'verilog-mode-hook 'jcs-verilog-mode-hook)

(add-to-list 'auto-mode-alist '("\\.v'?\\'" . verilog-mode))
