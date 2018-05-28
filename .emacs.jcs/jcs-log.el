;; ========================================================================
;; $File: jcs-log.el $
;; $Date: 2018-01-04 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Some Debug util.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;
;; TOPIC(jenchieh): How to preserve color in *Messages* buffer?
;; SOURCE(jenchieh): https://emacs.stackexchange.com/questions/20171/how-to-preserve-color-in-messages-buffer
;;
(defun jcs-message (format &rest args)
  "Acts like `message' but preserves string properties in the *Messages* buffer.
FORMAT : output format.
ARGS : arguments."
  (let ((message-log-max nil))
    (apply 'message format args))
  (with-current-buffer (get-buffer "*Messages*")
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (unless (zerop (current-column))
          ;; NOTE(jenchieh): no line break for this implementation.
          ;;(insert "\n")
          )
        (insert (apply 'format format args))
        ;; NOTE(jenchieh): no line break for this implementation.
        ;;(insert "\n")
        )))
  ;; NOTE(jenchieh): Do some stuff after logging message.
  (jcs-do-after-log-action))


(defun jcs-do-after-log-action ()
  "Action do after doing log."
  (save-selected-window
    (unless (string= (buffer-name) "*Messages*")
      (jcs-ensure-switch-to-buffer-other-window "*Messages*"))
    (when (string= (buffer-name) "*Messages*")
      (goto-char (point-max)))))

(defun jcs-log (format &rest args)
  "Log a log message.
FORMAT : output format.
ARGS : arguments."
  (jcs-message "\n$=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=$\n")
  (jcs-message "$ Log : ")
  (ignore-errors
    (let ((message-log-max nil))
      (apply 'message format args))
    (with-current-buffer (get-buffer "*Messages*")
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (unless (zerop (current-column)))
          (insert (apply 'format format args))))))
  (jcs-message "\n$=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=$\n"))

(defun jcs-error (format &rest args)
  "Log a error message.
FORMAT : output format.
ARGS : arguments."
  (jcs-message "\n$=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=$\n")
  (jcs-message "$ Error : ")
  (ignore-errors
    (let ((message-log-max nil))
      (apply 'message format args))
    (with-current-buffer (get-buffer "*Messages*")
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (unless (zerop (current-column)))
          (insert (apply 'format format args))))))
  (jcs-message "\n$=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=$\n"))

(defun jcs-warning (format &rest args)
  "Log a warning message.
FORMAT : output format.
ARGS : arguments."
  (jcs-message "\n$=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=$\n")
  (jcs-message "$ Warning : ")
  (ignore-errors
    (let ((message-log-max nil))
      (apply 'message format args))
    (with-current-buffer (get-buffer "*Messages*")
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (unless (zerop (current-column)))
          (insert (apply 'format format args))))))
  (jcs-message "\n$=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=$\n"))


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
