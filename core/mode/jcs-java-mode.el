;;; jcs-java-mode.el --- Java mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'javadoc-lookup)
(require 'organize-imports-java)

(setq javadoc-lookup-completing-read-function #'completing-read)

(defconst jcs-java-source-dir-name "src"
  "Java source directory name.")

(defun jcs-java-insert-package-from-src ()
  "Insert package string from `src' directory."
  (let ((tmp-file-name (buffer-file-name))
        tmp-split-dir-string
        (tmp-split-dir-string-size -1)
        (tmp-insert-package-path-string ""))
    ;; See if `src' directory contain in the string.
    (when (string-match-p jcs-java-source-dir-name tmp-file-name)

      ;; split the string by using string -> `src'.
      (setq tmp-split-dir-string (split-string tmp-file-name jcs-java-source-dir-name))

      ;; get the length of the split string
      (setq tmp-split-dir-string-size (length tmp-split-dir-string))

      ;; NOTE: Get the last part of the path.
      ;; If there are two `src' directory in the path.
      ;; We will just like to get the last part of the file path
      ;; and ignore whatever infront of them.
      (setq tmp-insert-package-path-string
            ;; Get the last part of the file path array.
            (nth (1- tmp-split-dir-string-size) tmp-split-dir-string))

      ;; Now we remove the file name from last part of path..
      (setq tmp-insert-package-path-string
            (s-replace (jcs-file-name) "" tmp-insert-package-path-string))

      ;; NOTE: If the string is less than two. Meaning the
      ;; file is directly under `default package'/`src' directory.
      (when (<= 2 (length tmp-insert-package-path-string))
        ;; Replace all `/' to `.'.
        (setq tmp-insert-package-path-string (s-replace
                                              "/"
                                              "."
                                              tmp-insert-package-path-string))

        ;; Remove the first and last character -> `.'.
        (setq tmp-insert-package-path-string
              (subseq tmp-insert-package-path-string
                      ;; Starting position, in order to remove the
                      ;; first character we start the string from
                      ;; index 1 instead of 0.
                      1
                      ;; End position, simply just minus one from
                      ;; length.
                      (1- (length tmp-insert-package-path-string))))

        ;; Add a `;' semi-colon at the end of the ready insert string.
        (setq tmp-insert-package-path-string
              (concatenate 'string tmp-insert-package-path-string ";"))

        ;; Insert the final package string.
        (insert "package ")
        (insert tmp-insert-package-path-string)
        (insert "\n")))))

(defun jcs-java-insert-package-src ()
  "Insert the package source declaration."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (jcs-current-word-equal-p "package")
      ;; kill the old package source declaration.
      (jcs-kill-whole-line))
    ;; insert the new one.
    (jcs-java-insert-package-from-src)
    (jcs-keep-one-line-between)))


(defun jcs-java-organize-imports ()
  "Organize all the import package.
Including adding or removing the package path."
  (interactive)
  (jcs-java-insert-package-src)  ; first organize package declaration.
  (organize-imports-java-do-imports))


(defsubst jcs-java-reload-local-source-paths-on-first-save ()
  "Reload local source paths on the first save."
  (when (jcs-file-directory-exists-p (buffer-file-name))
    (organize-imports-java-reload-local-source-paths)))

(defun jcs-java-untabify-save-buffer ()
  "Java untabify save."
  (interactive)
  (let (first-save)
    (unless (jcs-file-directory-exists-p (buffer-file-name))
      (setq first-save t))
    (ignore-errors (jcs-untabify-save-buffer))
    (when first-save
      (organize-imports-java-reload-local-source-paths))))

(defun jcs-java-tabify-save-buffer ()
  "Java tabify save."
  (interactive)
  (let (first-save)
    (unless (jcs-file-directory-exists-p (buffer-file-name))
      (setq first-save t))
    (ignore-errors (jcs-tabify-save-buffer))
    (when first-save
      (organize-imports-java-reload-local-source-paths))))

;;
;; (@* "Faces" )
;;

(defun jcs-init-java-faces ()
  "Initialize Java mode faces highlihgting."
  (let ((missing-modes '(java-mode)) (case-fold-search t))
    (dolist (mode missing-modes)
      (font-lock-add-keywords
       mode
       '(("^[ ]*\\([A-Z][a-zA-Z0-9_-]*\\)[a-zA-Z0-9._-]*\\.[a-zA-Z0-9_-]*[(]" 1 'font-lock-type-face t)
         ("\\([A-Z][a-zA-Z0-9._-]*\\)\\.[a-zA-Z0-9_-]*[),:]" 1 'font-lock-type-face t)
         ("[=][ \t\n]+\\(null\\)" 1 'jcs-font-lock-null-face t)
         ("[:=][ \t\n]+\\(void\\)" 1 'jcs-font-lock-null-face t)
         ("return[ \t\n]+\\(null\\)" 1 'jcs-font-lock-null-face t))
       'end))))

(jcs-init-java-faces)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-java-template ()
  "Header for Java header file."
  (jcs--file-header--insert "java" "default.txt"))

;;
;; (@* "Hook" )
;;

(defun jcs-java-mode-hook ()
  "Java mode hook."

  (setq-local docstr-show-type-name nil)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]java")
                              'jcs-insert-java-template)

  ;; Normal
  (jcs-bind-key (kbd "DEL") #'jcs-electric-backspace)
  (jcs-bind-key (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (jcs-bind-key (kbd ";") #'jcs-vs-semicolon-key)

  ;; switch window
  (jcs-bind-key "\ew" #'jcs-other-window-next)
  (jcs-bind-key (kbd "M-q") #'jcs-other-window-prev)

  ;; imports/package declaration.
  (jcs-bind-key (kbd "C-S-o") #'jcs-java-organize-imports)

  ;; javadoc
  (jcs-bind-key (kbd "<f2>") #'javadoc-lookup)
  (jcs-bind-key (kbd "S-<f2>") #'javadoc-lookup))

(add-hook 'java-mode-hook 'jcs-java-mode-hook)

(provide 'jcs-java-mode)
;;; jcs-java-mode.el ends here
