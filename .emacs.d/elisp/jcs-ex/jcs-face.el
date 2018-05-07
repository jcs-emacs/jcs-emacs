;; ========================================================================
;; $File: jcs-face.el $
;; $Date: 2017-12-13 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


(defface jcs-font-lock-comment-face
  '((t (:foreground "olive drab")))
  "JCS local comment face."
  :group 'basic-faces)
(defvar jcs-font-lock-comment-face 'jcs-font-lock-comment-face)

(defface jcs-font-lock-string-face
  '((t (:foreground "#D69D78")))
  "JCS local string face."
  :group 'basic-faces)
(defvar jcs-font-lock-string-face 'jcs-font-lock-string-face)

(defface jcs-font-lock-preprocessor-face
  '((t (:foreground "#8D9B99")))
  "JCS local preprocessor face."
  :group 'basic-faces)
(defvar jcs-font-lock-preprocessor-face 'jcs-font-lock-preprocessor-face)
