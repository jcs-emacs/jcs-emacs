;;; line-indicators-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "line-indicators" "line-indicators.el" (23841
;;;;;;  62468 0 0))
;;; Generated autoloads from line-indicators.el

(autoload 'line-indicators-transfer-to-saved-lines "line-indicators" "\
Transfer the `change-lines' to `saved-lines'.

\(fn)" t nil)

(autoload 'line-indicators-clear-indicators-lines-sign "line-indicators" "\
Clear all the indicator lines' sign.

\(fn)" t nil)

(autoload 'line-indicators-mode "line-indicators" "\
Minor mode 'line-indicators-mode'.

\(fn &optional ARG)" t nil)

(defvar global-line-indicators-mode nil "\
Non-nil if Global Line-Indicators mode is enabled.
See the `global-line-indicators-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-line-indicators-mode'.")

(custom-autoload 'global-line-indicators-mode "line-indicators" nil)

(autoload 'global-line-indicators-mode "line-indicators" "\
Toggle Line-Indicators mode in all buffers.
With prefix ARG, enable Global Line-Indicators mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Line-Indicators mode is enabled in all buffers where
`line-indicators-turn-on-line-indicators-mode' would do it.
See `line-indicators-mode' for more information on Line-Indicators mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "line-indicators" '(#("line-indicators-" 0 16 (fontified nil)))))

;;;***

(provide 'line-indicators-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; line-indicators-autoloads.el ends here
