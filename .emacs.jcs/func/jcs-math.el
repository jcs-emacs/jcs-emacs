;;; jcs-math.el --- Math Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


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
  "Make sure the IN-VAL is between IN-MIN and IN-MAX."
  (let ((out-result in-val))
    (cond ((<= in-val in-min) (progn (setq out-result in-min)))
          ((>= in-val in-max) (progn (setq out-result in-max))))
    out-result))

(defun jcs-in-range-p (in-val in-min in-max)
  "Check if IN-VAL in the range."
  (and (>= in-val in-min)
       (<= in-val in-max)))


(provide 'jcs-math)
;;; jcs-math.el ends here
