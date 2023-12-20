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
      (when (file-exists-p file)
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
  (let ((default `(("Default (empty)" . "Empty file")))
        (names (delete-dups (license-templates-keys)))
        (data))
    (mapc (lambda (file) (push (cons file "") data)) names)
    (append default (reverse data)))
  (pcase index
    (0 )
    (_ (license-templates-insert source))))

;;; Change Log

(defconst jcs-changelog-template-dir
  (concat user-emacs-directory "templates/__changelog/")
  "Path point to all changelog template files.")

(file-header-defsrc jcs-ask-insert-changelog-content "Type of the changelog: "
  ;; Ask to insert the changelog content base on SOURCE.
  (let ((default `(("Default (empty)" . "Empty file")))
        (files (jcs-dir-to-filename jcs-changelog-template-dir ".txt"))
        (data))
    (mapc (lambda (file) (push (cons file "") data)) files)
    (append default (reverse data)))
  (pcase source
    ("Default (empty)" )  ; Do nothing...
    (_ (file-header-insert-template-by-file-path
        (format "%s%s.txt" jcs-changelog-template-dir source)))))

;;
;; (@* "Hooks" )
;;

(jcs-add-hook 'text-mode-hook
  (setq-local electric-pair-open-newline-between-pairs nil)

  (company-fuzzy-backend-add-before 'company-kaomoji 'company-dabbrev)

  (jcs-insert-header-if-valid
   '("\\(/\\|\\`\\)[Ll][Ii][Cc][Ee][Nn][Ss][Ee]"
     "\\(/\\|\\`\\)[Cc][Oo][Pp][Yy][Ii][Nn][Gg]")
   'jcs-ask-insert-license-content
   :interactive t)
  (jcs-insert-header-if-valid
   '("\\(/\\|\\`\\)[Cc][Hh][Aa][Nn][Gg][Ee][-_]*[Ll][Oo][Gg]")
   'jcs-ask-insert-changelog-content
   :interactive t))

;;
;; (@* "Extensions" )
;;

(use-package flycheck-grammarly    :hook (flycheck-mode . flycheck-grammarly-setup))
(use-package flycheck-languagetool :hook (flycheck-mode . flycheck-languagetool-setup))

(use-package most-used-words
  :init
  (setq most-used-words-display-type 'table
        most-used-words-word-display 100))
