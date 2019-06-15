;;; jcs-log.el --- Debug Utils.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;
;; TOPIC: How to preserve color in *Messages* buffer?
;; SOURCE: https://emacs.stackexchange.com/questions/20171/how-to-preserve-color-in-messages-buffer
;;
(defun jcs-message (fmt &rest args)
  "Acts like `message' but preserves string properties in the *Messages* buffer.
FMT : output format.
ARGS : arguments."
  (let ((message-log-max nil))
    (apply 'message fmt args))
  (with-current-buffer (get-buffer "*Messages*")
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (apply 'format fmt args)))))
  (jcs-do-after-log-action))


(defun jcs-do-after-log-action ()
  "Action do after doing log."
  (save-selected-window
    (unless (string= (buffer-name) "*Messages*")
      (jcs-ensure-switch-to-buffer-other-window "*Messages*"))
    (when (string= (buffer-name) "*Messages*")
      (goto-char (point-max)))))


(defun jcs--log (title clean fmt &rest args)
  "Log a log message.
FMT : output format.
ARGS : arguments."
  (when clean
    (save-selected-window
      (unless (string= (buffer-name) "*Messages*")
        (jcs-ensure-switch-to-buffer-other-window "*Messages*"))
      (jcs-message-erase-buffer-stay)))
  (jcs-message "\n$=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=$\n")
  (jcs-message "$ %s : %s" title (apply 'format fmt args))
  (jcs-message "\n$=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=$\n"))


(defun jcs-log (fmt &rest args)
  "Log a log message.
FMT : output format.
ARGS : arguments."
  (apply 'jcs--log "Log" nil fmt args))

(defun jcs-log-clean (fmt &rest args)
  "Log a log message.
FMT : output format.
ARGS : arguments."
  (apply 'jcs--log "Log" t fmt args))


(defun jcs-log-list (list &optional in-prefix-msg in-val-del)
  "Log out a list.
LIST: list to log out.
IN-PREFIX-MSG : prefix message.
IN-VAL-DEL : value delimiter."
  (let ((count 0)
        (prefix-msg in-prefix-msg)
        (val-del in-val-del))
    ;; Set defult prefix message.
    (unless in-prefix-msg
      (setq prefix-msg "Index "))

    ;; Set default delimiter.
    (unless in-val-del
      (setq val-del " => "))

    (dolist (tmp-str list)
      (jcs-log "%s%s%s%s"
               prefix-msg  ;; Prefix Message
               count       ;; Index/Count
               val-del     ;; Index and Value Delimiter
               tmp-str)    ;; Value in current index
      (setq count (1+ count)))))


(provide 'jcs-log)
;;; jcs-log.el ends here
