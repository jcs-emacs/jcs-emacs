;; ========================================================================
;; $File: jcs-math.el $
;; $Date: 2018-06-19 12:20:44 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


(defun jcs-clamp-integer (in-val in-min in-max)
  "Make sure the value in the range.
IN-VAL : input value.
IN-MIN : input minimum value.
IN-MAX : input maximum value"
  (let ((out-result in-val))
    (cond ((<= in-val in-min) (progn (setq out-result in-min)))
          ((>= in-val in-max) (progn (setq out-result in-max))))
    out-result))
