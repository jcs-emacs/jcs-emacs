;; ========================================================================
;; $File: jcs-python-mode.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Python mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'python-mode)
(defun jcs-python-mode-hook ()
  ;; enable the stuff you want for Python here
  (electric-pair-mode nil)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (defun jcs-python-class-format ()
    "Format the given file as a Python file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-python-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]py" buffer-file-name) (jcs-python-class-format))
          ))

  ;; jcs python key binding
  (define-key python-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key python-mode-map "\C-c\C-c" #'kill-ring-save)
  (define-key python-mode-map [C-backspace] #'jcs-backward-delete-word)

  (define-key python-mode-map [M-up] #'jcs-previous-blank-line)
  (define-key python-mode-map [M-down] #'jcs-next-blank-line)

  (define-key python-mode-map "\C-k\C-f" #'jcs-py-indent-region)
  (define-key python-mode-map "\C-k\C-d" #'jcs-py-format-document)
  (define-key python-mode-map (kbd "C-S-f") #'jcs-py-format-region-or-document)

  ;; Edit
  (define-key python-mode-map (kbd "<up>") #'jcs-py-indent-up)
  (define-key python-mode-map (kbd "<down>") #'jcs-py-indent-down)
  (define-key python-mode-map (kbd "SPC") #'jcs-py-space)
  (define-key python-mode-map (kbd "S-SPC") #'jcs-py-real-space)
  (define-key python-mode-map (kbd "<backspace>") #'jcs-py-backspace)
  (define-key python-mode-map (kbd "S-<backspace>") #'jcs-py-real-backspace)

  ;; Comment
  (define-key python-mode-map (kbd "\"") #'jcs-py-maybe-insert-codedoc)
  )
(add-hook 'python-mode-hook 'jcs-python-mode-hook)

(add-to-list 'auto-mode-alist '("\\.py'?\\'" . python-mode))

(require 'elpy)
;;(elpy-enable)

(require 'ein)
;;(elpy-use-ipython)

;; enable autopep8 formatting on save
(require 'py-autopep8)
;;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
