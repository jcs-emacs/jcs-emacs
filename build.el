;; ========================================================================
;; $File: build.el $
;; $Date: 2018-11-25 20:35:16 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


(load-file "~/.emacs.jcs/jcs-package.el")

;; Install all needed packages without asking.
(jcs-ensure-package-installed jcs-package-install-list t)


;; Start regular Emacs file.
(load-file "~/.emacs")
