;; ========================================================================
;; $File: jcs-trans-window.el $
;; $Date: 2017-05-31 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


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


;;;###autoload
(defun jcs-increment-frame-transparent ()
  "Increment the frame transparency by 5 percent."
  (interactive)

  (let ((alpha (frame-parameter nil 'alpha)))
    (setq current-transparency (cond ((numberp alpha) alpha)
                                     ((numberp (cdr alpha)) (cdr alpha))
                                     ;; Also handle undocumented (<active> <inactive>) form.
                                     ((numberp (cadr alpha)) (cadr alpha))))

    (cond ((eql current-transparency 0)
           (progn (set-frame-parameter nil 'alpha '(5 . 5))))
          ((eql current-transparency 5)
           (progn (set-frame-parameter nil 'alpha '(10 . 10))))
          ((eql current-transparency 10)
           (progn (set-frame-parameter nil 'alpha '(15 . 15))))
          ((eql current-transparency 15)
           (progn (set-frame-parameter nil 'alpha '(20 . 20))))
          ((eql current-transparency 20)
           (progn (set-frame-parameter nil 'alpha '(25 . 25))))
          ((eql current-transparency 25)
           (progn (set-frame-parameter nil 'alpha '(30 . 30))))
          ((eql current-transparency 30)
           (progn (set-frame-parameter nil 'alpha '(35 . 35))))
          ((eql current-transparency 35)
           (progn (set-frame-parameter nil 'alpha '(40 . 40))))
          ((eql current-transparency 40)
           (progn (set-frame-parameter nil 'alpha '(45 . 45))))
          ((eql current-transparency 45)
           (progn (set-frame-parameter nil 'alpha '(50 . 50))))
          ((eql current-transparency 50)
           (progn (set-frame-parameter nil 'alpha '(55 . 55))))
          ((eql current-transparency 55)
           (progn (set-frame-parameter nil 'alpha '(60 . 60))))
          ((eql current-transparency 60)
           (progn (set-frame-parameter nil 'alpha '(65 . 65))))
          ((eql current-transparency 65)
           (progn (set-frame-parameter nil 'alpha '(70 . 70))))
          ((eql current-transparency 70)
           (progn (set-frame-parameter nil 'alpha '(75 . 75))))
          ((eql current-transparency 75)
           (progn (set-frame-parameter nil 'alpha '(80 . 80))))
          ((eql current-transparency 80)
           (progn (set-frame-parameter nil 'alpha '(85 . 85))))
          ((eql current-transparency 85)
           (progn (set-frame-parameter nil 'alpha '(90 . 90))))
          ((eql current-transparency 90)
           (progn (set-frame-parameter nil 'alpha '(95 . 95))))
          ((eql current-transparency 95)
           (progn (set-frame-parameter nil 'alpha '(100 . 100)))))))

;;;###autoload
(defun jcs-decrement-frame-transparent ()
  "Decrement the frame transparency by 5 percent."
  (interactive)

  (let ((alpha (frame-parameter nil 'alpha)))
    (setq current-transparency (cond ((numberp alpha) alpha)
                                     ((numberp (cdr alpha)) (cdr alpha))
                                     ;; Also handle undocumented (<active> <inactive>) form.
                                     ((numberp (cadr alpha)) (cadr alpha))))

    (cond ((eql current-transparency 100)
           (progn (set-frame-parameter nil 'alpha '(95 . 95))))
          ((eql current-transparency 95)
           (progn (set-frame-parameter nil 'alpha '(90 . 90))))
          ((eql current-transparency 90)
           (progn (set-frame-parameter nil 'alpha '(85 . 85))))
          ((eql current-transparency 85)
           (progn (set-frame-parameter nil 'alpha '(80 . 80))))
          ((eql current-transparency 80)
           (progn (set-frame-parameter nil 'alpha '(75 . 75))))
          ((eql current-transparency 75)
           (progn (set-frame-parameter nil 'alpha '(70 . 70))))
          ((eql current-transparency 70)
           (progn (set-frame-parameter nil 'alpha '(65 . 65))))
          ((eql current-transparency 65)
           (progn (set-frame-parameter nil 'alpha '(60 . 60))))
          ((eql current-transparency 60)
           (progn (set-frame-parameter nil 'alpha '(55 . 55))))
          ((eql current-transparency 55)
           (progn (set-frame-parameter nil 'alpha '(50 . 50))))
          ((eql current-transparency 50)
           (progn (set-frame-parameter nil 'alpha '(45 . 45))))
          ((eql current-transparency 45)
           (progn (set-frame-parameter nil 'alpha '(40 . 40))))
          ((eql current-transparency 40)
           (progn (set-frame-parameter nil 'alpha '(35 . 35))))
          ((eql current-transparency 35)
           (progn (set-frame-parameter nil 'alpha '(30 . 30))))
          ((eql current-transparency 30)
           (progn (set-frame-parameter nil 'alpha '(25 . 25))))
          ((eql current-transparency 25)
           (progn (set-frame-parameter nil 'alpha '(20 . 20))))
          ((eql current-transparency 20)
           (progn (set-frame-parameter nil 'alpha '(15 . 15))))
          ((eql current-transparency 15)
           (progn (set-frame-parameter nil 'alpha '(10 . 10))))
          ((eql current-transparency 10)
           (progn (set-frame-parameter nil 'alpha '(5 . 5))))
          ((eql current-transparency 5)
           (progn (set-frame-parameter nil 'alpha '(0 . 0)))))))
