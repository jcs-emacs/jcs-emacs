;;; jcs-log.el --- Debug Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Entry" )
;;

;; TOPIC: How to preserve color in *Messages* buffer?
;; SOURCE: https://emacs.stackexchange.com/questions/20171/how-to-preserve-color-in-messages-buffer

(defun jcs-message (fmt &rest args)
  "Log a message with FMT and ARGS.

Acts like `message' but preserves text properties in the *Messages* buffer."
  (jcs-no-log-apply (apply 'message fmt args))
  (with-current-buffer (get-buffer "*Messages*")
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (apply 'format fmt args))))))

(defun jcs-log (fmt &rest args)
  "Log a message with FMT and ARGS."
  (apply 'jcs--log "INFO" nil fmt args))

(defun jcs-log-clean (fmt &rest args)
  "Log a message with FMT and ARGS in the clean way."
  (apply 'jcs--log "INFO" t fmt args))

(defun jcs-print (&rest args)
  "Message out anything from ARGS."
  (jcs-unmute-apply
    (apply 'message (jcs-string-repeat "%s" (length args) " ") args)))

;;
;; (@* "List" )
;;

(defun jcs-log-list-clean (lst &optional in-prefix-msg in-val-del)
  "Log out a LST in a clean way.

For arguments IN-PREFIX-MSG and IN-VAL-DEL; see function `jcs-log-list'
for description."
  (jcs-do-before-log-action t)
  (apply 'jcs-log-list lst in-prefix-msg in-val-del))

(defun jcs-log-list (lst &optional in-prefix-msg in-val-del)
  "Log out the LST.

The LST object can either be list, vector, array, or hast-table.

Optional argument IN-PREFIX-MSG is the string added before each item.

Optional argument IN-VAL-DEL is string that point to item."
  (cond ((and (not (listp lst)) (not (vectorp lst)) (not (arrayp lst))
              (not (hash-table-p lst)))
         (user-error "[ERROR] Can't log list with this data object: %s" lst))
        ((hash-table-p lst)
         (jcs-log (json-encode lst)))
        ((>= 0 (length lst))
         (user-error "[WARNING] Can't log list with length lower than 0: %s" lst))
        (t
         (let ((count 0) (prefix-msg in-prefix-msg) (val-del in-val-del))
           (unless in-prefix-msg (setq prefix-msg "nth "))  ; Set defult prefix message.
           (unless in-val-del (setq val-del " => "))  ; Set default delimiter.
           (cond ((listp lst)
                  (dolist (tmp-str lst)
                    (jcs-log "%s%s%s`%s`"
                             prefix-msg  ; Prefix Message
                             count       ; Index/Count
                             val-del     ; Index and Value Delimiter
                             tmp-str)    ; Value in current index
                    (setq count (1+ count))))
                 (t
                  (mapc (lambda (tmp-str)
                          (jcs-log "%s%s%s`%s`"
                                   prefix-msg  ; Prefix Message
                                   count       ; Index/Count
                                   val-del     ; Index and Value Delimiter
                                   tmp-str)    ; Value in current index
                          (setq count (1+ count)))
                        lst)))))))

;;
;; (@* "Hooks" )
;;

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
    (if (string= (buffer-name) "*Messages*")
        (goto-char (point-max))
      (jcs-ensure-switch-to-buffer-other-window "*Messages*"))
    (jcs--message-buffer--first-load)))

;;
;; (@* "Util" )
;;

(defun jcs-sleep-for (&optional seconds milliseconds)
  "Wrap `sleep-for' function width default SECONDS and MILLISECONDS."
  (unless seconds (setq seconds jcs-sleep-for-seconds))
  (sleep-for seconds milliseconds))

(defun jcs-sit-for (&optional seconds nodisp)
  "Wrap `sit-for' function with default SECONDS and NODISP."
  (unless seconds (setq seconds jcs-sit-for-seconds))
  (sit-for seconds nodisp))

;;
;; (@* "Core" )
;;

(defun jcs--log (title clean fmt &rest args)
  "Log a message with TITLE, CLEAN, FMT and ARGS."
  (jcs-do-before-log-action clean)
  (jcs-message "╘[%s] %s\n" title (apply 'format fmt args))
  (jcs-do-after-log-action))

(provide 'jcs-log)
;;; jcs-log.el ends here
