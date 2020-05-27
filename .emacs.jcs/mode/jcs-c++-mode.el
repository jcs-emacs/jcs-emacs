;;; jcs-c++-mode.el --- C++ mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'jcs-cc-mode)

(defun jcs-unreal-c++-api-name ()
  "Return the name of the Unreal API for current file."
  (let* ((path (buffer-file-name))
         (dirs (split-string path "/" t))
         (api-name (jcs-find-item-in-list-offset dirs "Source" -1)))
    (concat api-name "_API")))

(defun jcs-unreal-c++-api-name-uppercase ()
  "Return the uppercase Unreal C++ API name."
  (upcase (jcs-unreal-c++-api-name)))

(defun jcs-unreal-c++-api-name-lowercase ()
  "Return the lowercase Unreal C++ API name."
  (downcase (jcs-unreal-c++-api-name)))

;;----------------------------------------------------------------------------

(defun jcs-c++-unreal-insert-header ()
  "Insert the Unreal C++ header depends on if is a header/source file."
  (let ((header-ext (append jcs-c++-header-extensions jcs-c-header-extensions))
        (source-ext (append jcs-c++-source-extensions jcs-c-source-extensions)))
    (jcs-insert-header-if-valid header-ext 'jcs-insert-c++-unreal-header-template)
    (jcs-insert-header-if-valid source-ext 'jcs-insert-c++-unreal-source-template)))

;;;###autoload
(defun jcs-c++-ask-source (sc)
  "Ask the source SC for editing C++ file."
  (interactive
   (list (completing-read
          "Major source for this C++ file: " '("Default"
                                               "Unreal Scripting"))))
  (cond ((string= sc "Default") (jcs-cc-insert-header))
        ((string= sc "Unreal Scripting") (jcs-c++-unreal-insert-header))))

;;----------------------------------------------------------------------------

(defun jcs-c++-mode-hook ()
  "C++ mode handling"

  ;; File Header
  (let ((ext-lst (append jcs-c-header-extensions jcs-c-source-extensions
                         jcs-c++-header-extensions jcs-c++-source-extensions)))
    (jcs-insert-header-if-valid ext-lst 'jcs-c++-ask-source t))

  ;; Normal
  (define-key c++-mode-map [f8] #'jcs-find-corresponding-file)
  (define-key c++-mode-map [S-f8] #'jcs-find-corresponding-file-other-window)

  ;; If just want to open the same file, not the corresponding file.
  (define-key c++-mode-map [f7] #'jcs-same-file-other-window)

  (define-key c++-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key c++-mode-map (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (define-key c++-mode-map (kbd "}") #'jcs-vs-closing-curly-bracket-key)
  (define-key c++-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; Comment Block.
  (define-key c++-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key c++-mode-map (kbd "*") #'jcs-c-comment-pair)

  ;; Comement
  (define-key c++-mode-map (kbd "C-k s") #'jcs-toggle-c-comment-style)

  (define-key c++-mode-map (kbd "#") #'jcs-vs-sharp-key)

  ;; Undo/Redo
  (define-key c++-mode-map (kbd "C-z") #'jcs-undo)
  (define-key c++-mode-map (kbd "C-y") #'jcs-redo))

(add-hook 'c++-mode-hook 'jcs-c++-mode-hook)

(provide 'jcs-c++-mode)
;;; jcs-c++-mode.el ends here
