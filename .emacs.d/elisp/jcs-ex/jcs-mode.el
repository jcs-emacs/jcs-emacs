;; This is the start of jcs-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2016 Jen-Chieh Shen

;; jcs-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh mode defines.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; Need global key defined first.
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-global-key.el")

;;---------------------------------------------
;; Trigger between command and inset mode
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
      (put 'jcs-insert-command-mode-toggle 'state t)))
  )

;;;###autoload
(defun jcs-depend-cross-mode-toggle()
  "Toggle depend/cross mode."
  (interactive)

  ;; NOTE(jenchieh): can only active when the minibuffer is
  ;; not active.
  (if (eq jcs-minibuffer-active nil)
      (progn

        (if (get 'jcs-local-online-mode-toggle 'state)
            (progn
              ;; depend mode
              (jcs-depend-mode)
              (put 'jcs-local-online-mode-toggle 'state nil))
          (progn
            ;; cross mode
            (jcs-cross-mode)
            (put 'jcs-local-online-mode-toggle 'state t)))
        )))

;;;###autoload
(defun jcs-reload-active-mode ()
  "Reload the active mode. Note this is opposite logic to the
toggle mode function."
  (interactive)

  (if (eq jcs-minibuffer-active nil)
      (if (get 'jcs-depend-cross-mode-toggle 'state)
          (progn
            ;; if state is true keep on cross mode.
            (jcs-cross-mode))
        (progn
          ;; vice versa, keep on depend mode.
          (jcs-depend-mode)))))

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
(call-interactively 'jcs-command-mode)


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
  "This mode depend on my own machine. More feature and more
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
  (define-key global-map "\C-f" 'helm-do-ag-this-file)
  (define-key global-map "\C-x\C-f" 'helm-do-ag-project-root)

  ;; Search
  (define-key global-map "\C-rp" 'jcs-ag-project-regexp)

  (jcs-global-key-rebind)
  )

;;;###autoload
(defun jcs-cross-mode ()
  "This mode run anywhere will work, usually less powerful then
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

  (jcs-global-key-rebind)
  )

;; NOTE(jayces): Since I often use my own machine, set the online
;; mode as the default.
(call-interactively 'jcs-depend-mode)

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-mode.el file
