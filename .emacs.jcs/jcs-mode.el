;;; jcs-mode.el --- Self mode defines.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;---------------------------------------------
;; Trigger between command and insert mode
;;---------------------------------------------

;;;###autoload
(defun jcs-insert-command-mode-toggle()
  "Toggle command/insert mode."
  (interactive)

  (if (get 'jcs-insert-command-mode-toggle 'state)
      (progn
        ;; command mode
        (jcs-command-mode)
        (put 'jcs-insert-command-mode-toggle 'state nil))
    (progn
      ;; insert mode
      (jcs-insert-mode)
      (put 'jcs-insert-command-mode-toggle 'state t))))

;;;###autoload
(defun jcs-depend-cross-mode-toggle()
  "Toggle depend/cross mode."
  (interactive)
  ;; NOTE(jenchieh): can only active when the minibuffer is
  ;; not active.
  (when (eq jcs-minibuffer-active nil)
    (if (get 'jcs-depend-cross-mode-toggle 'state)
        (progn
          ;; depend mode
          (jcs-depend-mode)
          (put 'jcs-depend-cross-mode-toggle 'state nil))
      (progn
        ;; cross mode
        (jcs-cross-mode)
        (put 'jcs-depend-cross-mode-toggle 'state t)))))

;;;###autoload
(defun jcs-reload-active-mode ()
  "Reload the active mode.  Note this is opposite logic to the \
toggle mode function."
  (interactive)
  ;; NOTE(jenchieh): can only active when the minibuffer is
  ;; not active.
  (when (eq jcs-minibuffer-active nil)
    (if (get 'jcs-depend-cross-mode-toggle 'state)
        ;; if state is true keep on cross mode.
        (jcs-cross-mode)
      ;; vice versa, keep on depend mode.
      (jcs-depend-mode))))


(defvar jcs-prompt-message-sleep-delay-time 0.4  ;; in seconds
  "Delay for a time for prompting out the message, so the user
can see the error/operation message.")

;;;###autoload
(defun jcs-helm-do-ag-this-file ()
  "Handle error for `helm-do-ag-this-file' command by switching
to the `jcs-cross-mode' in order to use cross mode search instead
of machine depenedent plugins/packages which is the `jcs-depend-mode'."
  (interactive)
  ;; NOTE(jenchieh): can only active when the minibuffer is
  ;; not active.
  (unless jcs-minibuffer-active
    (unless (ignore-errors (or (helm-do-ag-this-file) t))
      (jcs-cross-mode)
      (message "Error: This buffer is not visited file. Switch to cross mode search..")
      (sleep-for jcs-prompt-message-sleep-delay-time)
      (call-interactively 'isearch-forward))))

;;---------------------------------------------
;; Command Mode
;;---------------------------------------------

;;;###autoload
(defun jcs-command-mode()
  "In command mode. - JenChieh"
  (interactive)

  ;; set trigger
  (put 'jcs-insert-command-mode-toggle 'state nil)

  ;; switch to view mode
  ;;(view-mode-enable)

  ;; -----------------------------------------
  ;; Customize Theme
  ;; -----------------------------------------
  (jcs-gray-theme)

  ;; -----------------------------------------
  ;; Unset insert mode key
  ;;
  ;; NOTE(jenchieh): unset key should be
  ;; before of set keys
  ;; -----------------------------------------

  ;; -----------------------------------------
  ;; Set command mode key
  ;; -----------------------------------------

  )


;;---------------------------------------------
;; Insert Mode
;;---------------------------------------------

;;;###autoload
(defun jcs-insert-mode()
  "In insert mode. - JenChieh"
  (interactive)

  ;; set trigger
  (put 'jcs-insert-command-mode-toggle 'state t)

  ;; disable to view mode
  ;;(view-mode-disable)

  ;; -----------------------------------------
  ;; Customize Theme
  ;; -----------------------------------------
  (jcs-dark-green-theme)

  ;; -----------------------------------------
  ;; Unset command mode key
  ;;
  ;; NOTE(jenchieh): unset key should be
  ;; before of set keys
  ;; -----------------------------------------

  ;; -----------------------------------------
  ;; Set insert mode key
  ;; -----------------------------------------

  )

;; Make command mode start at the beginning.
(call-interactively #'jcs-command-mode)


;;------------------------------------------------------------------------------------------------------
;;; View Mode
;;------------------------------------------------------------------------------------------------------

(defun jcs-view-mode-hook()
  "In view mode, read only file."
  (interactive)

  ;; unset all the key
  (define-key view-mode-map "a" nil)
  (define-key view-mode-map "b" nil)
  (define-key view-mode-map "c" nil)
  (define-key view-mode-map "d" nil)
  (define-key view-mode-map "e" nil)
  (define-key view-mode-map "f" nil)
  (define-key view-mode-map "g" nil)
  (define-key view-mode-map "h" nil)
  (define-key view-mode-map "i" nil)
  (define-key view-mode-map "j" nil)
  (define-key view-mode-map "k" nil)
  (define-key view-mode-map "l" nil)
  (define-key view-mode-map "m" nil)
  (define-key view-mode-map "n" nil)
  (define-key view-mode-map "o" nil)
  (define-key view-mode-map "p" nil)
  (define-key view-mode-map "q" nil)
  (define-key view-mode-map "r" nil)
  (define-key view-mode-map "s" nil)
  (define-key view-mode-map "t" nil)
  (define-key view-mode-map "u" nil)
  (define-key view-mode-map "v" nil)
  (define-key view-mode-map "w" nil)
  (define-key view-mode-map "x" nil)
  (define-key view-mode-map "y" nil)
  (define-key view-mode-map "z" nil)
  (define-key view-mode-map "," nil)
  (define-key view-mode-map "\\" nil)
  (define-key view-mode-map "." nil)
  (define-key view-mode-map "," nil)
  (define-key view-mode-map "/" nil)
  (define-key view-mode-map "'" nil)
  (define-key view-mode-map " " nil)
  (define-key view-mode-map [tab] nil)
  (define-key view-mode-map (kbd "RET") nil)
  (define-key view-mode-map [space] nil)

  ;; just save buffer, don't care about the tab or spaces.
  (define-key view-mode-map "\C-s" 'save-buffer)
  )
(add-hook 'view-mode-hook 'jcs-view-mode-hook)


;;------------------------------------------------------------------------------------------------------
;;; Local Mode & Online Mode
;;------------------------------------------------------------------------------------------------------

;;;###autoload
(defun jcs-depend-mode ()
  "This mode depend on my own machine. More feature and more \
control of the editor."
  (interactive)

  ;; set toggle trigger
  (put 'jcs-depend-cross-mode-toggle 'state nil)

  ;; -----------------------------------------
  ;; Customize Theme
  ;; -----------------------------------------
  (jcs-gray-theme)

  ;; -----------------------------------------
  ;; Unset 'depend' mode key
  ;;
  ;; NOTE(jenchieh): unset key should be
  ;; before of set keys
  ;; -----------------------------------------
  (global-unset-key "\C-f")
  (global-unset-key "\C-r")

  ;; -----------------------------------------
  ;; Set 'depend' mode key
  ;; -----------------------------------------

  ;; search
  (define-key global-map "\C-f" 'jcs-helm-do-ag-this-file)
  (define-key global-map "\C-x\C-f" 'helm-do-ag-project-root)

  ;; Search
  (define-key global-map "\C-rp" 'jcs-ag-project-regexp)

  (when (functionp 'jcs-global-key-rebind)
    (jcs-global-key-rebind))
  )

;;;###autoload
(defun jcs-cross-mode ()
  "This mode run anywhere will work, usually less powerful then \
'jcs-depend-mode'."
  (interactive)

  ;; set toggle trigger
  (put 'jcs-depend-cross-mode-toggle 'state t)

  ;; -----------------------------------------
  ;; Customize Theme
  ;; -----------------------------------------
  (jcs-dark-green-theme)

  ;; -----------------------------------------
  ;; Unset 'cross' mode key
  ;;
  ;; NOTE(jenchieh): unset key should be
  ;; before of set keys
  ;; -----------------------------------------
  (global-unset-key "\C-f")
  (global-unset-key "\C-r")

  ;; -----------------------------------------
  ;; Set 'cross' mode key
  ;; -----------------------------------------

  ;; search
  (define-key global-map "\C-f" 'isearch-forward)
  (global-unset-key "\C-x\C-f")

  ;; Search
  (global-unset-key "\C-rp")

  (when (functionp 'jcs-global-key-rebind)
    (jcs-global-key-rebind))
  )


(provide 'jcs-mode)
;;; jcs-mode.el ends here
