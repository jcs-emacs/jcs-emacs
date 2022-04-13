;;; jcs-reporter.el --- Reporter process  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar jcs-process-reporter nil
  "Global instance process reporter.")

(defvar jcs-process-reporter-timer nil
  "Timer for process reporter.")

(defvar jcs-process-reporter-refresh 0.1
  "Process reporter's refresh rate.")

(defun jcs-process-reporter-start (&optional msg)
  "Start global process reporter with MSG displayed."
  (jcs-process-reporter-done)
  (unless msg (setq msg ""))
  (setq jcs-process-reporter (make-progress-reporter msg)
        jcs-process-reporter-timer (run-with-timer nil jcs-process-reporter-refresh #'jcs-process-reporter-update)))

(defun jcs-process-reporter-update (&optional value suffix)
  "Update global process reporter once."
  (when jcs-process-reporter
    (progress-reporter-update jcs-process-reporter value suffix)))

(defun jcs-process-reporter-done (&optional msg)
  "Kill global process reporter and log MSG when you are done."
  (when jcs-process-reporter
    (jcs-no-log-apply (progress-reporter-done jcs-process-reporter))
    (setq jcs-progress-reporter nil))
  (when (timerp jcs-process-reporter-timer)
    (cancel-timer jcs-process-reporter-timer)
    (setq jcs-process-reporter-timer nil)
    (jcs-no-log-apply (message msg))))

(provide 'jcs-reporter)
;;; jcs-reporter.el ends here
