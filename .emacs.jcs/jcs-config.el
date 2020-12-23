;;; jcs-config.el --- Your own configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Put your configuration code here ...

(with-eval-after-load 'dashboard
  ;; TODO: Delete this after patch fix!
  (defun dashboard-shorten-path-middle (path)
    "Shorten PATH from middle if exceeding maximum length."
    (let* ((len-path (length path)) (len-rep (length dashboard-path-shorten-string))
           (len-total (- dashboard-path-max-length len-rep))
           (center (/ len-total 2))
           (end-back center)
           (start-front (- len-path center))
           back front)
      (if (<= len-path dashboard-path-max-length) path
        (setq back (substring path 0 end-back)
              front (ignore-errors (substring path start-front len-path)))
        (if front (concat back dashboard-path-shorten-string front) "")))))

(provide 'jcs-config)
;;; jcs-config.el ends here
