;; ========================================================================
;; $File: jcs-math.el $
;; $Date: 2018-06-19 12:20:44 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


(defun jcs-to-positive (in-val)
  "To positive the value.
IN-VAL : Input value to set to positive."
  (when (and in-val
             (< in-val 0))
    (setq in-val (jcs-to-reverse in-val)))
  in-val)

(defun jcs-to-negative (in-val)
  "To negative the value.
IN-VAL : Input value to set to negative."
  (when (and in-val
             (> in-val 0))
    (setq in-val (jcs-to-reverse in-val)))
  in-val)

(defun jcs-is-positive (in-val)
  "Check if positive value.
IN-VAL : Input value to check if positive."
  (> in-val 0))

(defun jcs-is-negative (in-val)
  "Check if nagative value.
IN-VAL : Input value to check if negative."
  (< in-val 0))

(defun jcs-to-reverse (in-val)
  "To reverse the value.
IN-VAL : Input value to reverse."
  (- 0 in-val))

(defun jcs-to-reciprocal (in-val)
  "To reciprocal the value.
IN-VAL : Input value to reciprocal."
  (/ 1 in-val))

(defun jcs-clamp-integer (in-val in-min in-max)
  "Make sure the value in the range.
IN-VAL : Input value.
IN-MIN : Input minimum value.
IN-MAX : Input maximum value."
  (let ((out-result in-val))
    (cond ((<= in-val in-min) (progn (setq out-result in-min)))
          ((>= in-val in-max) (progn (setq out-result in-max))))
    out-result))
