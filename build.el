;; ========================================================================
;; $File: build.el $
;; $Date: 2018-11-25 20:35:16 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


;; Get the list of package dependencies.
(load-file "./.emacs.jcs/jcs-package.el")

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Install all needed packages without asking.
(jcs-ensure-package-installed jcs-package-install-list t)


;; Start regular Emacs file.
(load-file "./.emacs")
