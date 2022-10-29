;;; jcs-log.el --- Debug Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom jcs-log t
  "If non-nil, log out message."
  :type 'boolean
  :group 'jcs)

;;
;; (@* "Entry" )
;;

(defun jcs-log (fmt &rest args)
  "Log a message with FMT and ARGS."
  (apply 'jcs--log "INFO" nil fmt args))

(defun jcs-log-clean (fmt &rest args)
  "Log a message with FMT and ARGS in the clean way."
  (apply 'jcs--log "INFO" t fmt args))

(defun jcs-print (&rest args)
  "Message out anything from ARGS."
  (when jcs-log
    (msgu-unsilent
      (message (mapconcat (lambda (elm) (format "%s" elm)) args " ")))))

;;
;; (@* "List" )
;;

(defun jcs-log-list-clean (lst &optional prefix val-del)
  "Log out a LST in a clean way.

For arguments PREFIX and VAL-DEL; see function `jcs-log-list' for description."
  (jcs-log--before t)
  (apply 'jcs-log-list lst prefix val-del))

(defun jcs-log-list (lst &optional prefix val-del)
  "Log out the LST.

The LST object can either be list, vector, array, or hast-table.

Optional argument PREFIX is the string added before each item.

Optional argument VAL-DEL is string that point to item."
  (cond ((and (not (listp lst)) (not (vectorp lst)) (not (arrayp lst))
              (not (hash-table-p lst)))
         (user-error "[ERROR] Can't log list with this data object: %s" lst))
        ((hash-table-p lst)
         (jcs-log (json-encode lst)))
        ((>= 0 (length lst))
         (user-error "[WARNING] Can't log list with length lower than 0: %s" lst))
        (t
         (let ((prefix (or prefix "nth "))
               (val-del (or val-del " => "))
               (count 0))
           (cond ((listp lst)
                  (dolist (tmp-str lst)
                    (jcs-log "%s%s%s`%s`"
                             prefix  ; Prefix Message
                             count       ; Index/Count
                             val-del     ; Index and Value Delimiter
                             tmp-str)    ; Value in current index
                    (setq count (1+ count))))
                 (t
                  (mapc (lambda (tmp-str)
                          (jcs-log "%s%s%s`%s`"
                                   prefix  ; Prefix Message
                                   count       ; Index/Count
                                   val-del     ; Index and Value Delimiter
                                   tmp-str)    ; Value in current index
                          (setq count (1+ count)))
                        lst)))))))

;;
;; (@* "Core" )
;;

(defun jcs-log--before (clean)
  "Action do before doing log."
  (when clean
    (jcs-if-buffer-window (messages-buffer) (jcs-messages-erase-buffer)
      (save-selected-window
        (jcs-messages-other-window)
        (jcs-messages-erase-buffer)))))

(defun jcs-log--after ()
  "Action do after doing log."
  (jcs-when-buffer-window (messages-buffer) (goto-char (point-max))))

(defun jcs--log (title clean fmt &rest args)
  "Log a message with TITLE, CLEAN, FMT and ARGS."
  (when jcs-log
    (jcs-log--before clean)
    (msgu-color "╘[%s] %s\n" title (apply 'format fmt args))
    (jcs-log--after)))

;;
;; (@* "Extensions" )
;;

(leaf turbo-log
  :init
  (setq turbo-log-allow-insert-without-tree-sitter-p t))

(provide 'jcs-log)
;;; jcs-log.el ends here
