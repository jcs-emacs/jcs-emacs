;;; jcs-c++-mode.el --- C++ mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'jcs-cc-mode)

;;
;; (@* "Unreal Engine" )
;;

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

;;
;; (@* "Header" )
;;

(defun jcs-c++-unreal-insert-header (type)
  "Insert the Unreal C++ header depends on if is a header/source file."
  (let ((header-ext (append jcs-c++-header-extensions jcs-c-header-extensions))
        (source-ext (append jcs-c++-source-extensions jcs-c-source-extensions)))
    (pcase type
      ("Actor"
       (jcs-insert-header-if-valid header-ext 'jcs-insert-c++-unreal-header-template--actor)
       (jcs-insert-header-if-valid source-ext 'jcs-insert-c++-unreal-source-template--actor))
      ("ActorComponent"
       (jcs-insert-header-if-valid header-ext 'jcs-insert-c++-unreal-header-template--actor-component)
       (jcs-insert-header-if-valid source-ext 'jcs-insert-c++-unreal-source-template--actor-component)))))

(defun jcs-c++-ask-unreal-source-type (type)
  "Ask the source TYPE for Unreal C++ file."
  (interactive
   (list (completing-read
          "Type of Unreal C++ file: " '("Actor"
                                        "ActorComponent"))))
  (jcs-c++-unreal-insert-header type))

(defun jcs-c++-ask-source (sc)
  "Ask the source SC for editing C++ file."
  (interactive
   (list (completing-read
          "Major source for this C++ file: " '("Default"
                                               "Unreal Scripting"))))
  (pcase sc
    ("Default" (jcs-cc-insert-header))
    ("Unreal Scripting" (call-interactively #'jcs-c++-ask-unreal-source-type))))

;;
;; (@* "Templates" )
;;

(defun jcs-insert-c++-header-template ()
  "Header for C++ header file."
  (jcs--file-header--insert "c++" "header.txt"))

(defun jcs-insert-c++-source-template ()
  "Header for C++ source file."
  (jcs--file-header--insert "c++" "source.txt"))

(defun jcs-insert-c++-unreal-header-template--actor ()
  "Header for Unreal C++ header file with actor type."
  (jcs--file-header--insert "c++" "unreal/actor/header.txt"))

(defun jcs-insert-c++-unreal-source-template--actor ()
  "Header for Unreal C++ source file with actor type."
  (jcs--file-header--insert "c++" "unreal/actor/source.txt"))

(defun jcs-insert-c++-unreal-header-template--actor-component ()
  "Header for Unreal C++ header file with other type."
  (jcs--file-header--insert "c++" "unreal/actor-component/header.txt"))

(defun jcs-insert-c++-unreal-source-template--actor-component ()
  "Header for Unreal C++ source file with other type."
  (jcs--file-header--insert "c++" "unreal/actor-component/source.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'c++-mode-hook
  (add-hook 'docstr-after-insert-hook 'jcs-c++--docstr-after nil t)

  (jcs-company-safe-add-backend 'company-c-headers)

  ;; File Header
  (let ((ext-lst (append jcs-c-header-extensions jcs-c-source-extensions
                         jcs-c++-header-extensions jcs-c++-source-extensions)))
    (jcs-insert-header-if-valid ext-lst 'jcs-c++-ask-source :interactive t))

  ;; Normal
  (jcs-bind-key [f8] #'jcs-find-corresponding-file)
  (jcs-bind-key [S-f8] #'jcs-find-corresponding-file-other-window)

  ;; If just want to open the same file, not the corresponding file.
  (jcs-bind-key [f7] #'jcs-same-file-other-window)

  (jcs-bind-key (kbd "DEL") #'jcs-electric-backspace)
  (jcs-bind-key (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (jcs-bind-key (kbd ";") #'jcs-vs-semicolon-key)

  ;; Comement
  (jcs-bind-key (kbd "C-k s") #'jcs-toggle-c-comment-style)

  (jcs-bind-key (kbd "#") #'jcs-vs-sharp-key)

  ;; Undo/Redo
  (jcs-bind-key (kbd "C-z") #'jcs-undo)
  (jcs-bind-key (kbd "C-y") #'jcs-redo))

(provide 'jcs-c++-mode)
;;; jcs-c++-mode.el ends here