;; ========================================================================
;; $File: jcs-cmake-mode.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh CMake mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'cmake-mode)
(defun jcs-cmake-mode-hook ()
  ;;
  (electric-pair-mode nil)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  (defun jcs-makefile-format ()
    "Format the given file as a makefile. - JenChieh Makefile"

    (when (jcs-is-current-file-empty-p)
      (jcs-makefile-format-info)
      (goto-char (point-min))))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]makefile" buffer-file-name) (jcs-makefile-format))
        ((string-match "[Mm]akefile" buffer-file-name) (jcs-makefile-format))
        ((string-match "[.]mak" buffer-file-name) (jcs-makefile-format))
        )

  (defun jcs-cmake-format ()
    "Format the given file as a CMakeLists. - JenChieh CMake"
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-cmake-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "CMakeLists.txt" buffer-file-name) (jcs-cmake-format))
        )

  ;; Set Faces.
  (jcs-oop-init-set-face)

  ;; jcs makefile key binding
  (define-key cmake-mode-map (kbd "<up>") #'jcs-py-indent-up)
  (define-key cmake-mode-map (kbd "<down>") #'jcs-py-indent-down)
  (define-key cmake-mode-map (kbd "RET") #'jcs-makefile-newline)

  (define-key cmake-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key cmake-mode-map "\C-c\C-c" #'kill-ring-save)

  ;; tabify save key
  (define-key cmake-mode-map "\C-s" #'jcs-tabify-save-buffer)

  ;; Edit
  (define-key cmake-mode-map (kbd "SPC") #'jcs-py-space)
  (define-key cmake-mode-map (kbd "S-SPC") #'jcs-py-real-space)
  (define-key cmake-mode-map (kbd "<backspace>") #'jcs-py-backspace)
  (define-key cmake-mode-map (kbd "S-<backspace>") #'jcs-py-real-backspace)
  )
(add-hook 'cmake-mode-hook 'jcs-cmake-mode-hook)

(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)CMakeLists.txt" . cmake-mode))

;; temporary makefile
(add-to-list 'auto-mode-alist '("\\.mak?\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.makfile?\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)[Mm]akefile" . cmake-mode))

;; For autotools, autoconf, automake.
(add-to-list 'auto-mode-alist '("\\.ac?\\'" . cmake-mode))
