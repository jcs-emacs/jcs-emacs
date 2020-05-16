;;; jcs-log.el --- Debug Utils.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; TOPIC: How to preserve color in *Messages* buffer?
;; SOURCE: https://emacs.stackexchange.com/questions/20171/how-to-preserve-color-in-messages-buffer

(defun jcs-message (fmt &rest args)
  "Log a message with FMT and ARGS.
Acts like `message' but preserves string properties in the *Messages* buffer."
  (let ((message-log-max nil))
    (apply 'message fmt args))
  (with-current-buffer (get-buffer "*Messages*")
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (apply 'format fmt args))))))

(defun jcs-do-before-log-action (clean)
  "Action do before doing log."
  (when clean
    (save-selected-window
      (unless (string= (buffer-name) "*Messages*")
        (jcs-ensure-switch-to-buffer-other-window "*Messages*"))
      (jcs-message-erase-buffer-stay))))

(defun jcs-do-after-log-action ()
  "Action do after doing log."
  (save-selected-window
    (unless (string= (buffer-name) "*Messages*")
      (jcs-ensure-switch-to-buffer-other-window "*Messages*"))
    (when (string= (buffer-name) "*Messages*")
      (goto-char (point-max)))))


(defun jcs--log (title clean fmt &rest args)
  "Log a message with TITLE, CLEAN, FMT and ARGS."
  (jcs-do-before-log-action clean)
  (let ((seq-len (- (window-width) 2)))
    (jcs-message "╘[%s] %s\n" title (apply 'format fmt args)))
  (jcs-do-after-log-action))


(defun jcs-log (fmt &rest args)
  "Log a message with FMT and ARGS."
  (apply 'jcs--log "INFO" nil fmt args))

(defun jcs-log-clean (fmt &rest args)
  "Log a message with FMT and ARGS in the clean way."
  (apply 'jcs--log "INFO" t fmt args))


(defun jcs-log-list-clean (list &optional in-prefix-msg in-val-del)
  "Log out a LIST in clean *Messages* buffer with IN-PREFIX-MSG and IN-VAL-DEL."
  (jcs-do-before-log-action t)
  (apply 'jcs-log-list list in-prefix-msg in-val-del))

(defun jcs-log-list (list &optional in-prefix-msg in-val-del)
  "Log out a LIST.
IN-PREFIX-MSG : prefix message.
IN-VAL-DEL : value delimiter."
  (let ((count 0)
        (prefix-msg in-prefix-msg)
        (val-del in-val-del))
    (unless in-prefix-msg (setq prefix-msg "nth "))  ; Set defult prefix message.
    (unless in-val-del (setq val-del " => "))  ; Set default delimiter.
    (dolist (tmp-str list)
      (jcs-log "%s%s%s`%s`"
               prefix-msg  ; Prefix Message
               count       ; Index/Count
               val-del     ; Index and Value Delimiter
               tmp-str)    ; Value in current index
      (setq count (1+ count)))))

(provide 'jcs-log)
;;; jcs-log.el ends here
