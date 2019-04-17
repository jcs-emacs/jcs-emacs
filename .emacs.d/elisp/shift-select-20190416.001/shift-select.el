;;; shift-select.el --- Shift select text like how other text editor does.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-04-16 11:41:51

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Shift select text like how other text editor does.
;; Keyword: region select shift
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jcs090218/shift-select

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Shift select text like how other text editor does.
;;

;;; Code:

(defgroup shift-select nil
  "Shift select text like how other text editor does."
  :prefix "shift-select-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/shift-select"))


(defvar-local shift-select-start-pt -1
  "Shift select record point.")

(defvar-local shift-select-active nil
  "Shift select active.")

(defvar-local shift-select-shift-pressed nil
  "Check if shift is pressed.")


(defun shift-select-pre-command-hook ()
  "Shift select pre command hook."
  (setq-local shift-select-shift-pressed
              (ignore-errors
                (and (stringp (symbol-name last-command-event))
                     (string-match-p "S-" (symbol-name last-command-event)))))
  (when (or this-command-keys-shift-translated
            shift-select-shift-pressed)
    (setq-local shift-select-start-pt (point))))

(defun shift-select-post-command-hook ()
  "Shift select post command hook."
  (if (or this-command-keys-shift-translated
          shift-select-shift-pressed)
      (let ((cur-pt (point)))
        ;; User moves the cursor when holding shift key.
        (unless (= cur-pt shift-select-start-pt)
          (unless shift-select-active
            (push-mark shift-select-start-pt)
            (goto-char cur-pt)
            (setq-local mark-active t)
            (setq-local shift-select-active t))))
    (when (and mark-active
               shift-select-active)
      (pop-mark))
    (setq-local shift-select-active nil)))


(defun shift-select-enable ()
  "Enable `shift-select' in current buffer."
  (add-hook 'pre-command-hook 'shift-select-pre-command-hook nil t)
  (add-hook 'post-command-hook 'shift-select-post-command-hook nil t))

(defun shift-select-disable ()
  "Disable `shift-select-mode' in current buffer."
  (remove-hook 'pre-command-hook 'shift-select-pre-command-hook t)
  (remove-hook 'post-command-hook 'shift-select-post-command-hook t))


;;;###autoload
(define-minor-mode shift-select-minor-mode
  "Minor mode 'shift-select-mode'."
  :lighter " ShiSel"
  :group shift-select
  (if shift-select-minor-mode
      (shift-select-enable)
    (shift-select-disable)))

(defun shift-select-turn-on-shift-select-mode ()
  "Turn on the 'shift-select-mode'."
  (shift-select-minor-mode 1))

;;;###autoload
(define-globalized-minor-mode global-shift-select-mode
  shift-select-minor-mode shift-select-turn-on-shift-select-mode
  :require 'shift-select)


(provide 'shift-select)
;;; shift-select.el ends here
