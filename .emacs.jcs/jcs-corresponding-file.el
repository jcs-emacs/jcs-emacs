;; ========================================================================
;; $File: jcs-corresponding-file.el $
;; $Date: 2018-05-15 16:06:18 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; IMPORTANT(jenchieh): Keep core function at the top
;; of this file.
;;
;;   * `jcs-find-corresponding-file'
;;   * `jcs-find-corresponding-file-other-window'
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;;###autoload
(defun jcs-find-corresponding-file ()
  "Find the file that corresponds to this one."
  (interactive)
  (let ((corresponding-file-name ""))
    ;;;
    ;; NOTE(jenchieh): Add your corresponding file here.
    ;;;

    ;; NOTE(jenchieh): Find C/C++ corresponding file.
    (when (or (jcs-is-current-major-mode-p "c++-mode")
              (jcs-is-current-major-mode-p "c-mode"))
      (setq corresponding-file-name (jcs-cc-corresponding-file)))

    ;; NOTE(jenchieh): Find WEB corresponding file.
    (when (or
           ;; For ASP.NET -> [file-name].aspx.cs
           (jcs-is-current-major-mode-p "csharp-mode")
           ;; For ASP.NET -> [file-name].aspx
           (jcs-is-current-major-mode-p "web-mode"))
      (setq corresponding-file-name (jcs-web-corresponding-file)))

    ;; Error check before return it value.
    (if corresponding-file-name (find-file corresponding-file-name)
      (error "Unable to find a corresponding file.."))))

;;;###autoload
(defun jcs-find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (find-file-other-window buffer-file-name)
  (jcs-find-corresponding-file)
  (other-window -1))


;;-----------------------------------------------------------
;; C/C++
;;-----------------------------------------------------------

(defun jcs-cc-corresponding-file ()
  "Find the corresponding file for C/C++ file."
  (let ((corresponding-file-name "")
        (tmp-base-file-name (file-name-sans-extension buffer-file-name)))
    (cond ((string-match "\\.hin" buffer-file-name)
           (progn
             (setq corresponding-file-name (concat tmp-base-file-name ".cin"))))
          ((string-match "\\.hpp" buffer-file-name)
           (progn
             (setq corresponding-file-name (concat tmp-base-file-name ".cpp"))))
          ((string-match "\\.h" buffer-file-name)
           (progn
             (if (file-exists-p (concat tmp-base-file-name ".c"))
                 (setq corresponding-file-name (concat tmp-base-file-name ".c"))
               (setq corresponding-file-name (concat tmp-base-file-name ".cpp")))))
          ((string-match "\\.cin" buffer-file-name)
           (progn
             (setq corresponding-file-name (concat tmp-base-file-name ".hin"))))
          ((string-match "\\.cpp" buffer-file-name)
           (progn
             (setq corresponding-file-name (concat tmp-base-file-name ".h"))))
          ((string-match "\\.c" buffer-file-name)
           (progn
             (setq corresponding-file-name (concat tmp-base-file-name ".h"))))
          )
    ;; Return file name.
    corresponding-file-name))


;;-----------------------------------------------------------
;; Web Related
;;-----------------------------------------------------------

(defun jcs-web-corresponding-file ()
  "Find the corresponding file for WEB related file."
  (let ((corresponding-file-name "")
        (tmp-base-file-name (file-name-sans-extension buffer-file-name)))
    (cond ((string-match "\\.aspx.cs" buffer-file-name)
           (progn
             (setq corresponding-file-name tmp-base-file-name)))
          ((string-match "\\.aspx" buffer-file-name)
           (progn
             (setq corresponding-file-name (concat tmp-base-file-name ".aspx.cs"))))
          )

    ;; Return file name.
    corresponding-file-name))
