;;; shift-select-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "shift-select" "shift-select.el" (23742 49343
;;;;;;  0 0))
;;; Generated autoloads from shift-select.el

(autoload 'shift-select-minor-mode "shift-select" "\
Minor mode 'shift-select-mode'.

\(fn &optional ARG)" t nil)

(defvar global-shift-select-mode nil "\
Non-nil if Global Shift-Select mode is enabled.
See the `global-shift-select-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-shift-select-mode'.")

(custom-autoload 'global-shift-select-mode "shift-select" nil)

(autoload 'global-shift-select-mode "shift-select" "\
Toggle Shift-Select minor mode in all buffers.
With prefix ARG, enable Global Shift-Select mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Shift-Select minor mode is enabled in all buffers where
`shift-select-turn-on-shift-select-mode' would do it.
See `shift-select-minor-mode' for more information on Shift-Select minor mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "shift-select" '(#("shift-select-" 0 13 (fontified nil)))))

;;;***

(provide 'shift-select-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; shift-select-autoloads.el ends here
