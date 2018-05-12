;; ========================================================================
;; $File: jcs-txt-mode.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Text mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;====================================
;; normal text mode
;;---------------------------
(require 'gitignore-mode)
(defun jcs-gitignore-mode-hook ()
  "JayCeS Gitignore mode."

  (electric-pair-mode nil)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  (defun jcs-text-related-format ()
    "Format the given file as a text related. - JenChieh Text file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-txt-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]txt" buffer-file-name) (jcs-text-related-format))
        ((string-match "[.]gitignore" buffer-file-name) (jcs-text-related-format))
        ((string-match "[.]gitattributes" buffer-file-name) (jcs-text-related-format))
        )

  ;; jcs gitignore key binding
  (define-key gitignore-mode-map (kbd "<up>") 'previous-line)
  (define-key gitignore-mode-map (kbd "<down>") 'next-line)
  (define-key gitignore-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key gitignore-mode-map "\C-c\C-c" 'kill-ring-save)
  (define-key gitignore-mode-map (kbd "<up>") 'previous-line)
  (define-key gitignore-mode-map (kbd "<down>") 'next-line)
  )
(add-hook 'gitignore-mode-hook 'jcs-gitignore-mode-hook)

;; git file types
(add-to-list 'auto-mode-alist '("\\.gitignore?\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.gitattributes?\\'" . gitignore-mode))

;; temporary mode for all Text related!
(add-to-list 'auto-mode-alist '("\\.bin?\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.md?\\'" . gitignore-mode))

;; temporary ALGOL
(add-to-list 'auto-mode-alist '("\\.alg?\\'" . gitignore-mode))

;; key-binding for normal text mode.
(define-key global-map "\C-xg" 'gitignore-mode)


;;====================================
;; Org mode.
;;---------------------------
(require 'org)

;; No fold when open `org' file.
(setq org-startup-folded nil)

(defun jcs-org-mode()
  "JayCeS org mode."

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  (defun jcs-org-mode-format()
    "Fromat the given file as a text file related. - JenChieh Text file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-txt-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]txt" buffer-file-name) (jcs-org-mode-format))
        )

  ;; jcs org mode key binding
  (define-key org-mode-map (kbd "<up>") 'previous-line)
  (define-key org-mode-map (kbd "<down>") 'next-line)
  (define-key org-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key org-mode-map "\C-c\C-c" 'kill-ring-save)
  (define-key org-mode-map "\C-a" 'mark-whole-buffer)
  (define-key org-mode-map [tab] 'jcs-tab-key)
  (define-key org-mode-map [C-tab] 'org-cycle)

  (define-key org-mode-map (kbd "S-<up>") 'jcs-org-table-up)
  (define-key org-mode-map (kbd "S-<down>") 'jcs-org-table-down)
  (define-key org-mode-map (kbd "S-<left>") 'jcs-org-table-left)
  (define-key org-mode-map (kbd "S-<right>") 'jcs-org-table-right)
  )
(add-hook 'org-mode-hook 'jcs-org-mode)

;; set the defualt text mode to org mode.
(add-to-list 'auto-mode-alist '("\\.txt?\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.ini?\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.properties?\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)README" . org-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)LICENSE" . org-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)bochsrc" . org-mode))
