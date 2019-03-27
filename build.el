;;; build.el --- Test the configuration.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Get the list of package dependencies.
(load-file "./.emacs.jcs/jcs-package.el")

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Install all needed packages without asking.
(jcs-ensure-package-installed jcs-package-install-list t)


;; Start regular Emacs file.
;;(load-file "./.emacs")


;;(provide 'build)
;;; build.el ends here
