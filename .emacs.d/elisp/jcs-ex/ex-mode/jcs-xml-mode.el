;; ========================================================================
;; $File: jcs-js-mode.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh XML mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun jcs-xml-mode-hook ()

  (interactive)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for XML here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  (defun jcs-xml-format ()
    "Format the given file as a XML file. - JenChieh XML file."
    (when (is-current-file-empty-p)
      (jcs-insert-xml-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]xml" buffer-file-name) (jcs-xml-format))
        )
  )
;; NOTE(jenchieh): they ae using nxml-mode instead of xml-mode
;; which is really weird.
(add-hook 'nxml-mode-hook 'jcs-xml-mode-hook)
(add-hook 'nxml-mode-hook 'emmet-mode)

(add-to-list 'auto-mode-alist '("\\.xml?\\'" . xml-mode))
