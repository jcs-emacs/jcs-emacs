;;; line-indicators.el --- Line annotation similar to Visual Studio.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-03-10 11:36:02

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Line annotation similar to Visual Studio.
;; Keyword: annotation indicator line-number
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (indicators "0.0.4"))
;; URL: https://github.com/jcs090218/line-indicators

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Line annotation similar to Visual Studio.
;;

;;; Code:

(require 'indicators)


(defgroup line-indicators nil
  "Visual Studio like line annotation in Emacs."
  :prefix "line-indicators-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/line-indicators"))


(defface line-indicators-modified-sign-face
  `((t :foreground "#EFF284"))
  "Modifed sign face."
  :group 'line-indicators)

(defface line-indicators-saved-sign-face
  `((t :inherit linum
       :foreground "#577430"))
  "Modifed sign face."
  :group 'line-indicators)

(defcustom line-indicators-ignore-buffer-names '("*Backtrace*"
                                                 "*Buffer List*"
                                                 "*Checkdoc Status*"
                                                 "*Echo Area"
                                                 "*helm"
                                                 "*Help*"
                                                 "magit"
                                                 "*Minibuf-"
                                                 "*Packages*"
                                                 "*run*"
                                                 "*shell*"
                                                 "*undo-tree*")
  "Buffer Name list you want to ignore this mode."
  :type 'list
  :group 'line-indicators)

(defcustom line-indicators-fringe-placed 'left-fringe
  "Line indicators fringe location."
  :type 'symbol
  :group 'line-indicators)

(defcustom line-indicators-fringe 'filled-rectangle
  "Line indicators fringe symbol."
  :type 'symbol
  :group 'line-indicators)


;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defvar-local line-indicators-change-lines '()
  "List of line that change in current temp buffer.")

(defvar-local line-indicators-saved-lines '()
  "List of line that saved in current temp buffer.")

(defvar-local line-indicators-before-begin-pt -1
  "Record down the before begin point.")

(defvar-local line-indicators-before-end-pt -1
  "Record down the before end point.")

(defvar-local line-indicators-before-begin-linum -1
  "Record down the before begin line number.")

(defvar-local line-indicators-before-end-linum -1
  "Record down the before end line number.")

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defun line-indicators-total-line ()
  "Return current buffer's maxinum line."
  (line-number-at-pos (point-max)))

(defun line-indicators-is-contain-list-string (in-list in-str)
  "Check if a string contain in any string in the string list.
IN-LIST : list of string use to check if IN-STR in contain one of
the string.
IN-STR : string using to check if is contain one of the IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string-match-p (regexp-quote lb-sub-str) in-str)) in-list))

(defun line-indicators-is-contain-list-integer (in-list in-int)
  "Check if a integer contain in any string in the string list.
IN-LIST : list of integer use to check if IN-INT in contain one of the integer.
IN-INT : integer using to check if is contain one of the IN-LIST."
  (cl-some #'(lambda (lb-sub-int) (= lb-sub-int in-int)) in-list))


(defun line-indicators-mark-line-by-linum-at-pos (fc)
  "Mark the line by using line number.
FC : Face to apply."
  (line-indicators-mark-line-by-linum (line-number-at-pos) fc))

(defun line-indicators-mark-line-by-linum (ln fc)
  "Mark the line by using line number.
LN : Line number.
FC : Face to apply."
  (ind-create-indicator-at-line ln
                                :managed t
                                :dynamic t
                                :relative nil
                                :fringe line-indicators-fringe-placed
                                :bitmap line-indicators-fringe
                                :face fc))

(defun line-indicators-mark-line-by-point (pt fc)
  "Mark the line by using line point.
PT : Point.
FC : Face to apply."
  (ind-create-indicator pt
                        :managed t
                        :dynamic t
                        :relative nil
                        :fringe line-indicators-fringe-placed
                        :bitmap line-indicators-fringe
                        :face fc))


;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defun line-indicators-is-valid-line-indicators-situation (&optional begin end)
  "Check if is valid to apply line indicator at the moment.
BEGIN : start changing point.
END : end changing point."
  (if (and begin
           end)
      (and (not buffer-read-only)
           (not (line-indicators-is-contain-list-string line-indicators-ignore-buffer-names
                                                        (buffer-name)))
           (<= begin (point-max))
           (<= end (point-max)))
    (and (not buffer-read-only)
         (not (line-indicators-is-contain-list-string line-indicators-ignore-buffer-names
                                                      (buffer-name))))))

(defun line-indicators-delta-list-lines-by-bound (in-list bound delta)
  "Delta the count of line in the list by bound.
IN-LIST : input line list.
BOUND : everything above is affective by delta.
DELTA : addition/subtraction value of the line count."
  (let ((index 0))
    (dolist (tmp-linum in-list)
      (when (< bound tmp-linum)
        (setf (nth index in-list) (+ tmp-linum delta)))
      (setq index (1+ index))))
  in-list)

(defun line-indicators-delta-list-lines-by-bound-once (bound-current-line delta-line-count)
  "Delta list line by bound once to all type of lines using by this package.
BOUND-CURRENT-LINE  : Center line number.
DELTA-LINE-COUNT : Delta line count."
  ;; Add up delta line count to `change-lines' list.
  (setq-local line-indicators-change-lines
              (line-indicators-delta-list-lines-by-bound line-indicators-change-lines
                                                         bound-current-line
                                                         delta-line-count))
  ;; Add up delta line count to `saved-lines' list.
  (setq-local line-indicators-saved-lines
              (line-indicators-delta-list-lines-by-bound line-indicators-saved-lines
                                                         bound-current-line
                                                         delta-line-count)))

(defun line-indicators-remove-lines-out-range (in-list)
  "Remove all the line in the list that are above the last/maxinum line \
or less than zero line in current buffer.
IN-LIST : list to be remove or take effect with."
  ;; Remove line that are above last/max line in buffer.
  (let ((last-line-in-buffer (line-indicators-total-line))
        (tmp-lst in-list))
    (dolist (line in-list)
      ;; If is larger than last/max line in buffer.
      (when (or (< last-line-in-buffer line)
                (<= line 0))
        ;; Remove line because we are deleting.
        (setq tmp-lst (remove line tmp-lst))))
    tmp-lst))

(defun line-indicators-remove-lines-out-range-once ()
  "Do `line-indicators-remove-lines-out-range' to all line list apply to this mode."
  (setq-local line-indicators-change-lines (line-indicators-remove-lines-out-range line-indicators-change-lines))
  (setq-local line-indicators-saved-lines (line-indicators-remove-lines-out-range line-indicators-saved-lines)))

;;;###autoload
(defun line-indicators-transfer-to-saved-lines ()
  "Transfer the `change-lines' to `saved-lines'."
  (interactive)
  (setq-local line-indicators-saved-lines
              (append line-indicators-saved-lines line-indicators-change-lines))
  ;; Clear the change lines.
  (setq-local line-indicators-change-lines '())

  ;; Removed save duplicates
  (delete-dups line-indicators-saved-lines)
  ;; Remove out range.
  (line-indicators-remove-lines-out-range-once)

  (line-indicators-mark-buffer))

(defun line-indicators-mark-buffer ()
  "Mark the whole buffer."
  (save-excursion
    (ind-clear-indicators-absolute)
    (dolist (ln line-indicators-change-lines)
      (line-indicators-mark-line-by-linum ln 'line-indicators-modified-sign-face))
    (dolist (ln line-indicators-saved-lines)
      (line-indicators-mark-line-by-linum ln 'line-indicators-saved-sign-face))))

(defun line-indicators-before-change-functions (begin end)
  "Do stuff before buffer is changed.
BEGIN : beginning of the changes.
END : end of the changes."
  (when (line-indicators-is-valid-line-indicators-situation begin end)
    (setq-local line-indicators-before-begin-pt begin)
    (setq-local line-indicators-before-end-pt end)
    (setq-local line-indicators-before-begin-linum (line-number-at-pos begin))
    (setq-local line-indicators-before-end-linum (line-number-at-pos end))))

(defun line-indicators-after-change-functions (begin end length)
  "Do stuff after buffer is changed.
BEGIN : beginning of the changes.
END : end of the changes.
LENGTH : deletion length."
  (when (line-indicators-is-valid-line-indicators-situation begin end)
    (save-excursion
      ;; When begin and end are not the same, meaning the there
      ;; is addition/deletion happening in the current buffer.
      (let ((begin-linum -1)
            (end-linum -1)
            (delta-line-count 0)
            ;; Current line is the bound. Is the line after we do
            ;; either addition/subtraction.
            (bound-current-line -1)
            ;; Is deleting line or adding new line?
            (is-deleting-line nil))

        ;; Is deleting line can be depends on the length.
        (when  (= begin end)
          (setq is-deleting-line t))
        (if is-deleting-line
            (progn
              (setq begin line-indicators-before-begin-pt)
              (setq end line-indicators-before-end-pt)
              (setq begin-linum line-indicators-before-begin-linum)
              (setq end-linum line-indicators-before-end-linum))
          (setq end-linum (line-number-at-pos end))
          (setq begin-linum (line-number-at-pos begin)))

        (goto-char begin)

        (setq delta-line-count (- end-linum begin-linum))
        (when is-deleting-line
          (setq delta-line-count (- 0 delta-line-count)))

        ;; Just add the current line.
        (push begin-linum line-indicators-change-lines)

        ;; If adding line, bound is the begin line number.
        (setq bound-current-line begin-linum)


        (when (or (not (= begin-linum end-linum))
                  (not (= delta-line-count 0)))
          (line-indicators-remove-lines-out-range-once)

          ;; NOTE(jenchieh): Deletion..
          (when is-deleting-line
            (let ((current-linum begin-linum)
                  (record-last-linum begin-linum)
                  (reach-last-line-in-buffer nil))
              (while (and (< current-linum end-linum)
                          ;; Cannot be the same as last line in buffer.
                          (not reach-last-line-in-buffer))
                ;; To do the next line.
                (forward-line 1)
                (setq current-linum (line-number-at-pos))

                ;; Remove line because we are deleting.
                (setq-local line-indicators-change-lines
                            (remove current-linum line-indicators-change-lines))
                (setq-local line-indicators-saved-lines
                            (remove current-linum line-indicators-saved-lines))

                ;; NOTE: Check if we need to terminate this loop?
                (when (or
                       ;; Check if still the same as last line.
                       (= current-linum record-last-linum)
                       ;; Check if current linum last line in buffer
                       (= current-linum (line-indicators-total-line)))
                  (setq reach-last-line-in-buffer t))

                ;; Update the last linum, make sure it won't do the same
                ;; line twice.
                (setq record-last-linum current-linum)))

            (line-indicators-delta-list-lines-by-bound-once bound-current-line
                                                            delta-line-count))

          ;; NOTE: Addition..
          (when (and (not is-deleting-line)
                     (not (= begin end))
                     (= length 0))
            (line-indicators-delta-list-lines-by-bound-once bound-current-line
                                                            delta-line-count)

            ;; Adding line. (After adding line/lines, we just need to loop
            ;; throught those lines and add it to `line-indicators-change-lines'
            ;; list.)
            (let ((current-linum begin-linum)
                  ;; Record down the last current line number, to make
                  ;; sure that we don't fall into infinite loop.
                  (record-last-linum begin-linum)
                  (reach-last-line-in-buffer nil))
              (while (and (<= current-linum end-linum)
                          ;; Cannot be the same as last line in buffer.
                          (not reach-last-line-in-buffer))

                ;; Push the current line to changes-line.
                (push current-linum line-indicators-change-lines)

                ;; To do the next line.
                (forward-line 1)
                (setq current-linum (line-number-at-pos))

                ;; NOTE(jenchieh): Check if we need to terminate this loop?
                (when (or
                       ;; Check if still the same as last line.
                       (= current-linum record-last-linum)
                       ;; Check if current linum last line in buffer
                       (= current-linum (line-indicators-total-line)))
                  (setq reach-last-line-in-buffer t))

                ;; Update the last linum, make sure it won't do the same
                ;; line twice.
                (setq record-last-linum current-linum)))))

        (delete-dups line-indicators-change-lines)
        (delete-dups line-indicators-saved-lines)

        ;; Remove out range.
        (line-indicators-remove-lines-out-range-once)

        (line-indicators-mark-buffer)))))


(defun line-indicators-enable ()
  "Enable `line-indicators' in current buffer."
  (add-hook 'before-change-functions #'line-indicators-before-change-functions nil t)
  (add-hook 'after-change-functions #'line-indicators-after-change-functions nil t)
  (advice-add 'save-buffer :after #'line-indicators-transfer-to-saved-lines))

(defun line-indicators-disable ()
  "Disable `line-indicators' in current buffer."
  (remove-hook 'before-change-functions #'line-indicators-before-change-functions t)
  (remove-hook 'after-change-functions #'line-indicators-after-change-functions t)
  (advice-remove 'save-buffer #'line-indicators-transfer-to-saved-lines)
  (line-indicators-clear-indicators-lines-sign))


;;;###autoload
(defun line-indicators-clear-indicators-lines-sign ()
  "Clear all the indicator lines' sign."
  (interactive)
  (setq-local line-indicators-change-lines '())
  (setq-local line-indicators-saved-lines '())
  (ind-clear-indicators-absolute))


;;;###autoload
(define-minor-mode line-indicators-mode
  "Minor mode 'line-indicators-mode'."
  :lighter " LI"
  :group line-indicators
  (if line-indicators-mode
      (line-indicators-enable)
    (line-indicators-disable)))

(defun line-indicators-turn-on-line-indicators-mode ()
  "Turn on the 'line-indicators-mode'."
  (line-indicators-mode 1))

;;;###autoload
(define-globalized-minor-mode global-line-indicators-mode
  line-indicators-mode line-indicators-turn-on-line-indicators-mode
  :require 'line-indicators)


(provide 'line-indicators)
;;; line-indicators.el ends here
