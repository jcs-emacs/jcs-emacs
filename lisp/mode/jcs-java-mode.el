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
        (tmp-split-dir-string-size -1)
        (tmp-insert-package-path-string "")
        tmp-split-dir-string)
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

(defun jcs-java--first-save ()
  "First save hook."
  (organize-imports-java-reload-local-source-paths)
  (remove-hook 'after-save-hook #'jcs-java--first-save t))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-java-template "java" "default.txt"
  "Header for Java header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'java-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  ;; File Header
  (jcs-insert-header-if-valid '("[.]java")
                              'jcs-insert-java-template)

  (unless (jcs-file-directory-exists-p (buffer-file-name))
    (add-hook 'after-save-hook #'jcs-java--first-save nil t))

  (jcs-key-local
    `(((kbd "DEL")    . jcs-electric-backspace)
      ((kbd "<f2>")   . javadoc-lookup)
      ((kbd "S-<f2>") . javadoc-lookup))))

(provide 'jcs-java-mode)
;;; jcs-java-mode.el ends here
