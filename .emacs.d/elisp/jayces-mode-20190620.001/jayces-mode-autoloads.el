;;; jayces-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "jayces-mode" "jayces-mode.el" (23820 16516
;;;;;;  0 0))
;;; Generated autoloads from jayces-mode.el

(autoload 'jayces-mode "jayces-mode" "\
Major mode for editing JayCeS file.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.jcs'?\\'" . jayces-mode))

(add-to-list 'auto-mode-alist '("\\.jayces'?\\'" . jayces-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jayces-mode" '(#("jayces-" 0 7 (fontified nil)))))

;;;***

(provide 'jayces-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jayces-mode-autoloads.el ends here
