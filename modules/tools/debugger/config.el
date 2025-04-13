;;; tools/debugger/config.el  -*- lexical-binding: t; -*-

;; gdb
(setq gdb-show-main t
      gdb-many-windows t)

(jcs-add-hook '(dap-mode-hook)
  (let* ((mode-name (elenv-2str major-mode))
         (guess-req (s-replace "-mode" "" mode-name)))
    (require (intern (format "dap-%s" guess-req)) nil t)))

(defmacro jcs-debugger-cond-active (form1 form2)
  "Execute FROM1 or FORM2 depends on the active debugger."
  `(cond ((elenv-debugging-p) ,form1)
         ((jcs-debugging-p) ,form2)
         (t (message "[INFO] Invalid debugger action: `%s`"
                     (propertize (elenv-2str this-command)
                                 'face 'font-lock-type-face)))))

(defmacro jcs-debugger-cond-buffer (form1 form2 &rest form3)
  "Execute FROM1 or FORM2 depends on the current buffer."
  `(progn
     (jcs-require '(edebug dap-mode))
     (cond ((memq major-mode '(emacs-lisp-mode)) ,form1)
           (dap-mode                             ,form2)
           (t                                    ,@form3))))

(defun jcs-debug-toggle-breakpoint ()
  "Toggle breakpoint on/off."
  (interactive)
  (jcs-debugger-cond-buffer (edebug-toggle-disable-breakpoint)
                            (dap-breakpoint-toggle)))

(defun jcs-debug-start ()
  "Start debugging."
  (interactive)
  (jcs-debugger-cond-buffer (edebug-eval-top-level-form)
                            (call-interactively #'dap-debug)
                            (execrun-run)))

(defun jcs-debug-stop ()
  "Stop debugging."
  (interactive)
  (jcs-debugger-cond-active (edebug-stop)
                            (dap-disconnect)))

(defun jcs-debug-restart ()
  "Restart debugger."
  (interactive)
  (jcs-debugger-cond-active (progn (edebug-stop) (edebug-defun))
                            (dap-debug-restart)))

(defun jcs-debug-step-over ()
  "Step over."
  (interactive)
  (jcs-debugger-cond-active (edebug-forward-sexp)
                            (dap-next)))

(defun jcs-debug-step-in ()
  "Step in."
  (interactive)
  (jcs-debugger-cond-active (edebug-step-in)
                            (dap-step-in)))

(defun jcs-debug-step-out ()
  "Step out."
  (interactive)
  (jcs-debugger-cond-active (edebug-step-out)
                            (dap-step-out)))
