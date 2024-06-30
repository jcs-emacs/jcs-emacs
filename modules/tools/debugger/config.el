;;; tools/debugger/config.el  -*- lexical-binding: t; -*-

;; gdb
(setq gdb-show-main t
      gdb-many-windows t)

(jcs-add-hook '(dap-mode-hook)
  (let* ((mode-name (elenv-2str major-mode))
         (guess-req (s-replace "-mode" "" mode-name)))
    (require (intern (format "dap-%s" guess-req)) nil t)))

(defmacro jcs-debugger-cond (form1 form2)
  "Debugger action FROM1 and FORM2."
  `(cond ((elenv-debugging-p) ,@form1)
         ((jcs-debugging-p) ,@form2)
         (t (message "[INFO] Invalid debugger action: `%s`"
                     (propertize (elenv-2str this-command)
                                 'face 'font-lock-type-face)))))

(defun jcs-debug-toggle-break-point ()
  "Toggle break point."
  (interactive)
  (jcs-debugger-cond (edebug-toggle-disable-breakpoint)
                     (dap-breakpoint-toggle)))

(defun jcs-debug-stop ()
  "Stop debugging."
  (interactive)
  (jcs-debugger-cond (edebug-stop)
                     (dap-stop-thread)))

(defun jcs-debug-restart ()
  "Restart debugger."
  (interactive)
  (jcs-debugger-cond (progn (edebug-stop) (edebug-defun))
                     (dap-debug-restart)))

(defun jcs-debug-step-over ()
  "Step over."
  (interactive)
  (jcs-debugger-cond (edebug-forward-sexp)
                     (dap-next)))

(defun jcs-debug-step-in ()
  "Step in."
  (interactive)
  (jcs-debugger-cond (edebug-step-in)
                     (dap-step-in)))

(defun jcs-debug-step-out ()
  "Step out."
  (interactive)
  (jcs-debugger-cond (edebug-step-out)
                     (dap-step-out)))
