;;; jcs-message-func.el --- In *Message* buffer.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;###autoload
(defun jcs-message-erase-buffer ()
  "Erase the *Messages* buffer."
  (interactive)
  (let ((is-killed nil))
    ;; Kill it first.
    (setq is-killed (jcs-maybe-kill-this-buffer))

    ;; Message one message to retrieve `*Message*' buffer
    ;; prepare for next use. Or else it some operation
    ;; might prompt some issue that needed `*Message*'
    ;; buffer to be exists.
    (message "Retrieve *Message* buffer..")

    (when is-killed
      (save-selected-window
        (when (ignore-errors (jcs-jump-shown-to-buffer "*Buffer List*"))
          ;; NOTE: Refresh buffer menu once.
          (jcs-buffer-menu))))))

;;;###autoload
(defun jcs-message-erase-buffer-stay ()
  "Reopen *Messages* buffer."
  (interactive)
  (jcs-message-erase-buffer)
  (switch-to-buffer "*Messages*"))


(provide 'jcs-message-func)
;;; jcs-message-func.el ends here
