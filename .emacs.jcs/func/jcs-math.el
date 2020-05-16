;;; jcs-math.el --- Math Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun jcs-to-positive (in-val)
  "Convert IN-VAL to positive value."
  (when (and in-val
             (< in-val 0))
    (setq in-val (jcs-to-reverse in-val)))
  in-val)

(defun jcs-to-negative (in-val)
  "Convert IN-VAL to negative value."
  (when (and in-val
             (> in-val 0))
    (setq in-val (jcs-to-reverse in-val)))
  in-val)

(defun jcs-is-positive (in-val)
  "Check if IN-VAL a positive value."
  (> in-val 0))

(defun jcs-is-negative (in-val)
  "Check if IN-VAL a negative value."
  (< in-val 0))

(defun jcs-to-reverse (in-val)
  "Reverse value IN-VAL."
  (- 0 in-val))

(defun jcs-to-reciprocal (in-val)
  "Reciprocal value IN-VAL."
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
