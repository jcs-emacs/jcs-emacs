;;; emacs/text-mode/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Util" )
;;

(defun jcs-dir-to-filename (path &optional ext full with-ext)
  "Return list of filename by PATH.

Optional argument EXT is the extension filter.

If optional argument FULL is non-nil; return full path.
If optional argument WITH-EXT is non-nil; return path with extension."
  (let ((files (ignore-errors
                 (directory-files path t (when ext (format "\\%s$" ext)))))
        types fn)
    (dolist (file files)
      (when (jcs-file-p file)
        (setq fn (file-name-nondirectory file))
        (unless (member fn '("." ".."))
          (unless full (setq file fn))
          (unless with-ext (setq file (file-name-sans-extension file)))
          (push file types))))
    (sort types #'string-lessp)))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-text-template "text" "default.txt"
  "Header for Text header file.")

;;; License

(file-header-defsrc jcs-ask-insert-license-content "Type of the license: "
  ;; Ask to insert the license content base on SOURCE.
  (delete-dups
   (sort (append (list "Default (empty)") (license-templates-names))
         #'string-lessp))
  (cond ((string= source "Default (empty)") (progn ))
        ((member source (license-templates-names))
         (license-templates-insert source))))

;;; Change Log

(defconst jcs-changelog-template-dir
  (concat user-emacs-directory "templates/__changelog/")
  "Path point to all changelog template files.")

(file-header-defsrc jcs-ask-insert-changelog-content "Type of the changelog: "
  ;; Ask to insert the changelog content base on SOURCE.
  (append (list "Default (empty)")
          (jcs-dir-to-filename jcs-changelog-template-dir ".txt"))
  (pcase source
    ("Default (empty)" )  ; Do nothing...
    (_ (file-header-insert-template-by-file-path
        (format "%s%s.txt" jcs-changelog-template-dir source)))))

;;
;; (@* "Hooks" )
;;

(jcs-add-hook 'text-mode-hook
  (setq-local electric-pair-open-newline-between-pairs nil)

  (company-fuzzy-backend-add 'company-kaomoji)

  (jcs-insert-header-if-valid
   '("\\(/\\|\\`\\)[Ll][Ii][Cc][Ee][Nn][Ss][Ee]") 'jcs-ask-insert-license-content
   :interactive t)
  (jcs-insert-header-if-valid
   '("\\(/\\|\\`\\)[Cc][Hh][Aa][Nn][Gg][Ee][-_]*[Ll][Oo][Gg]")
   'jcs-ask-insert-changelog-content
   :interactive t))

;;
;; (@* "Extensions" )
;;

(leaf flycheck-grammarly      :hook (flycheck-mode-hook . flycheck-grammarly-setup))
(leaf flycheck-languagetool   :hook (flycheck-mode-hook . flycheck-languagetool-setup))

(leaf most-used-words
  :init
  (setq most-used-words-display-type 'table
        most-used-words-word-display 100))
