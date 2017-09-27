;; This is the start of jcs-trans-window.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-trans-window.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2017>
;; Time-stamp: <2017-05-31 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-trans-window is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-trans-window is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;----------------------------------------------
;; Transparent Toggle
;; -----------------------------------
;; TAG: transparent, opacity, mode.
;;----------------------------------------------
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

;;;###autoload
(defun jcs-toggle-transparency ()
  "Make the frame transparent.
SOURCE: https://www.emacswiki.org/emacs/TransparentEmacs"
  (interactive)

  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         ;; NOTE(jenchieh): Second parameter is transparency
         ;; when not focus.
         ;; (when focus, when not focus)
         '(80 . 80) '(100 . 100)))))


(setf jcs-frame-transparency 10)

;;;###autoload
(defun jcs-increment-frame-transparent ()
  "Increment the frame transparency by 5 percent."
  (interactive)

  (let ((alpha (frame-parameter nil 'alpha)))
    (setq current-transparency (cond ((numberp alpha) alpha)
                                     ((numberp (cdr alpha)) (cdr alpha))
                                     ;; Also handle undocumented (<active> <inactive>) form.
                                     ((numberp (cadr alpha)) (cadr alpha))))

    (if (eql current-transparency 0)
        (set-frame-parameter nil 'alpha '(5 . 5)))
    (if (eql current-transparency 5)
        (set-frame-parameter nil 'alpha '(10 . 10)))
    (if (eql current-transparency 10)
        (set-frame-parameter nil 'alpha '(15 . 15)))
    (if (eql current-transparency 15)
        (set-frame-parameter nil 'alpha '(20 . 20)))
    (if (eql current-transparency 20)
        (set-frame-parameter nil 'alpha '(25 . 25)))
    (if (eql current-transparency 25)
        (set-frame-parameter nil 'alpha '(30 . 30)))
    (if (eql current-transparency 30)
        (set-frame-parameter nil 'alpha '(35 . 35)))
    (if (eql current-transparency 35)
        (set-frame-parameter nil 'alpha '(40 . 40)))
    (if (eql current-transparency 40)
        (set-frame-parameter nil 'alpha '(45 . 45)))
    (if (eql current-transparency 45)
        (set-frame-parameter nil 'alpha '(50 . 50)))
    (if (eql current-transparency 50)
        (set-frame-parameter nil 'alpha '(55 . 55)))
    (if (eql current-transparency 55)
        (set-frame-parameter nil 'alpha '(60 . 60)))
    (if (eql current-transparency 60)
        (set-frame-parameter nil 'alpha '(65 . 65)))
    (if (eql current-transparency 65)
        (set-frame-parameter nil 'alpha '(70 . 70)))
    (if (eql current-transparency 70)
        (set-frame-parameter nil 'alpha '(75 . 75)))
    (if (eql current-transparency 75)
        (set-frame-parameter nil 'alpha '(80 . 80)))
    (if (eql current-transparency 80)
        (set-frame-parameter nil 'alpha '(85 . 85)))
    (if (eql current-transparency 85)
        (set-frame-parameter nil 'alpha '(90 . 90)))
    (if (eql current-transparency 90)
        (set-frame-parameter nil 'alpha '(95 . 95)))
    (if (eql current-transparency 95)
        (set-frame-parameter nil 'alpha '(100 . 100)))
    )
  )

;;;###autoload
(defun jcs-decrement-frame-transparent ()
  "Decrement the frame transparency by 5 percent."
  (interactive)

  (let ((alpha (frame-parameter nil 'alpha)))
    (setq current-transparency (cond ((numberp alpha) alpha)
                                     ((numberp (cdr alpha)) (cdr alpha))
                                     ;; Also handle undocumented (<active> <inactive>) form.
                                     ((numberp (cadr alpha)) (cadr alpha))))

    (if (eql current-transparency 100)
        (set-frame-parameter nil 'alpha '(95 . 95)))
    (if (eql current-transparency 95)
        (set-frame-parameter nil 'alpha '(90 . 90)))
    (if (eql current-transparency 90)
        (set-frame-parameter nil 'alpha '(85 . 85)))
    (if (eql current-transparency 85)
        (set-frame-parameter nil 'alpha '(80 . 80)))
    (if (eql current-transparency 80)
        (set-frame-parameter nil 'alpha '(75 . 75)))
    (if (eql current-transparency 75)
        (set-frame-parameter nil 'alpha '(70 . 70)))
    (if (eql current-transparency 70)
        (set-frame-parameter nil 'alpha '(65 . 65)))
    (if (eql current-transparency 65)
        (set-frame-parameter nil 'alpha '(60 . 60)))
    (if (eql current-transparency 60)
        (set-frame-parameter nil 'alpha '(55 . 55)))
    (if (eql current-transparency 55)
        (set-frame-parameter nil 'alpha '(50 . 50)))
    (if (eql current-transparency 50)
        (set-frame-parameter nil 'alpha '(45 . 45)))
    (if (eql current-transparency 45)
        (set-frame-parameter nil 'alpha '(40 . 40)))
    (if (eql current-transparency 40)
        (set-frame-parameter nil 'alpha '(35 . 35)))
    (if (eql current-transparency 35)
        (set-frame-parameter nil 'alpha '(30 . 30)))
    (if (eql current-transparency 30)
        (set-frame-parameter nil 'alpha '(25 . 25)))
    (if (eql current-transparency 25)
        (set-frame-parameter nil 'alpha '(20 . 20)))
    (if (eql current-transparency 20)
        (set-frame-parameter nil 'alpha '(15 . 15)))
    (if (eql current-transparency 15)
        (set-frame-parameter nil 'alpha '(10 . 10)))
    (if (eql current-transparency 10)
        (set-frame-parameter nil 'alpha '(5 . 5)))
    (if (eql current-transparency 5)
        (set-frame-parameter nil 'alpha '(0 . 0)))
    )
  )

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-trans-window.el file
