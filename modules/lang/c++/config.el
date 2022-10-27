;;; lang/c++/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Unreal Engine" )
;;

(defun jcs-unreal-c++-api-name ()
  "Return the name of the Unreal API for current file."
  (let* ((path (buffer-file-name))
         (dirs (split-string path "/" t))
         (api-name (jcs-find-item-in-list-offset dirs "Source" -1)))
    (concat api-name "_API")))

;;
;; (@* "Header" )
;;

;; Ask the source TYPE for Unreal C++ file.
(file-header-defsrc jcs-c++-ask-unreal-source-type "Type of Unreal C++ file: "
  '("Actor" "ActorComponent")
  (let ((header-ext (append jcs-c++-header-extensions jcs-c-header-extensions))
        (source-ext (append jcs-c++-source-extensions jcs-c-source-extensions)))
    (pcase index
      (0
       (jcs-insert-header-if-valid header-ext 'jcs-insert-c++-unreal-header-template--actor)
       (jcs-insert-header-if-valid source-ext 'jcs-insert-c++-unreal-source-template--actor))
      (1
       (jcs-insert-header-if-valid header-ext 'jcs-insert-c++-unreal-header-template--actor-component)
       (jcs-insert-header-if-valid source-ext 'jcs-insert-c++-unreal-source-template--actor-component)))))

;; Ask the source SC for editing C++ file.
(file-header-defsrc jcs-c++-ask-source "Major source for this C++ file: "
  '("Default" "Unreal Scripting")
  (pcase index
    (0 (jcs-cc-insert-header))
    (1 (call-interactively #'jcs-c++-ask-unreal-source-type))))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-c++-header-template "c++" "header.txt"
  "Header for C++ header file.")

(file-header-defins jcs-insert-c++-source-template "c++" "source.txt"
  "Header for C++ source file.")

(file-header-defins jcs-insert-c++-unreal-header-template--actor
    "c++" "unreal/actor/header.txt"
  "Header for Unreal C++ header file with actor type.")

(file-header-defins jcs-insert-c++-unreal-source-template--actor
    "c++" "unreal/actor/source.txt"
  "Header for Unreal C++ source file with actor type.")

(file-header-defins jcs-insert-c++-unreal-header-template--actor-component
    "c++" "unreal/actor-component/header.txt"
  "Header for Unreal C++ header file with other type.")

(file-header-defins jcs-insert-c++-unreal-source-template--actor-component
    "c++" "unreal/actor-component/source.txt"
  "Header for Unreal C++ source file with other type.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'c++-mode-hook
  (add-hook 'ts-docstr-after-insert-hook 'jcs-c++--ts-docstr-after nil t)

  (company-fuzzy-backend-add 'company-c-headers)

  ;; File Header
  (let ((ext-lst (append jcs-c-header-extensions jcs-c-source-extensions
                         jcs-c++-header-extensions jcs-c++-source-extensions)))
    (jcs-insert-header-if-valid ext-lst 'jcs-c++-ask-source :interactive t))

  (jcs-key-local
    `(((kbd "C-k s") . jcs-toggle-c-comment-style))))
