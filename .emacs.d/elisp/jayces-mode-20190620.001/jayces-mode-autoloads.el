;;; jayces-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "jayces-mode" "jayces-mode.el" (23819 41248
;;;;;;  0 0))
;;; Generated autoloads from jayces-mode.el

(autoload 'jayces-mode "jayces-mode" "\
Major mode for editing JayCeS file.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.jcs'?\\'" . jayces-mode))

(add-to-list 'auto-mode-alist '("\\.jayces'?\\'" . jayces-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jayces-mode" '(#("jayces-" 0 7 (fontified nil)))))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jayces-mode-autoloads.el ends here
