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

;;---------------------------------------------
;; Trigger between command and inset mode
;;---------------------------------------------

;;;###autoload
(defun jcs-mode-toggle()
  "Toggle command/insert mode."
  (interactive)

  (if (get 'jcs-mode-toggle 'state)
      (progn
        ;; command mode
        (jcs-command-mode)
        (put 'jcs-mode-toggle 'state nil))
    (progn
      ;; insert mode
      (jcs-insert-mode)
      (put 'jcs-mode-toggle 'state t)))
  )

;;---------------------------------------------
;; Command Mode
;;---------------------------------------------

;;;###autoload
(defun jcs-command-mode()
  "In command mode. - JenChieh"
  (interactive)

  ;; set trigger
  (put 'jcs-mode-toggle 'state nil)

  ;; switch to view mode
  ;;(view-mode-enable)

  ;; -----------------------------------------
  ;; Customize Theme
  ;; -----------------------------------------

  (set-foreground-color "#D2D2D2")
  (set-background-color "#161616")

  (set-cursor-color "#40FF40")
  (set-face-background 'hl-line "midnight blue")

  ;; set mode line
  (set-face-background 'mode-line "#BFBFBF")
  (set-face-background 'modeline-inactive "#4D4D4D")
  ;; set the vertical border
  (set-face-background 'vertical-border "#D2D2D2")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))

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
  (put 'jcs-mode-toggle 'state t)

  ;; disable to view mode
  ;;(view-mode-disable)

  ;; -----------------------------------------
  ;; Customize Theme
  ;; -----------------------------------------
  (set-foreground-color "#D2D2D2")
  (set-background-color "#161616")

  (set-cursor-color "#40FF40")
  (set-face-background 'hl-line "midnight blue")

  ;; set mode line
  (set-face-background 'mode-line "#467E7D")
  (set-face-background 'mode-line-inactive "#294645")
  ;; set the vertical border
  (set-face-background 'vertical-border "#467E7D")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))

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
;; This is the end of jcs-mode.el file
