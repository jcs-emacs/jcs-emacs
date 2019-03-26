;;; reload-emacs.el --- Reload Emacs without restart Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-03-12 16:49:36

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Reload Emacs without restart Emacs.
;; Keyword: convenience emacs reload restart
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/jcs090218/reload-emacs

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Reload Emacs without restart Emacs.
;;

;;; Code:


(defgroup reload-emacs nil
  "Reload Emacs without restart Emacs."
  :prefix "reload-emacs-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/reload-emacs"))


(defcustom reload-emacs-load-path '()
  "List of all the configuration of files."
  :group 'reload-emacs
  :type 'list)

(defcustom reload-emacs-before-hook nil
  "Hooks run before `reload-emacs' is run."
  :group 'reload-emacs
  :type 'hook)

(defcustom reload-emacs-after-hook nil
  "Hooks run after `reload-emacs' is run."
  :group 'reload-emacs
  :type 'hook)

(defvar reload-emacs-reloading nil
  "Flag to see if currently the Emacs is reloading.")


(defun reload-emacs-load-files (dir files)
  "Load files under a directory.
DIR : directory path.
FILES : files to load."
  (dolist (path files)
    (let ((filepath (concat (file-name-as-directory dir) path)))
      (load-file filepath))))

(defun reload-emacs-all-configs ()
  "Reload all Emacs configurations."
  (dolist (dir reload-emacs-load-path)
    (let ((files-el (directory-files dir nil "\\.el$"))
          (files-elc (directory-files dir nil "\\.elc$")))
      (reload-emacs-load-files dir files-el)
      (reload-emacs-load-files dir files-elc))))

;;;###autoload
(defun reload-emacs ()
  "Reload all Emacs configurations without restarting Emacs."
  (interactive)
  (run-hooks 'reload-emacs-before-hook)
  (save-match-data
    (save-window-excursion
      (save-selected-window
        (save-restriction
          (let ((reload-emacs-reloading t))
            ;; Load starting file.
            (cond ((file-exists-p "~/init.el")
                   (load-file "~/.init.el"))
                  ((file-exists-p "~/.emacs")
                   (load-file "~/.emacs")))
            (reload-emacs-all-configs))))))
  (run-hooks 'reload-emacs-after-hook))


(provide 'reload-emacs)
;;; reload-emacs.el ends here
