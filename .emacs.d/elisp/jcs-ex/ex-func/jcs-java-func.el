;; ========================================================================
;; $File: jcs-java-func.el $
;; $Date: 2018-04-07 15:09:05 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; When editing the Java related file.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defvar jcs-java-source-dir-name "src"
  "Java source directory name.")

(defun jcs-java-insert-package-from-src ()
  "Insert package string from `src' directory."
  (let ((tmp-file-name (buffer-file-name))
        (tmp-split-dir-string '())
        (tmp-split-dir-string-size -1)
        (tmp-insert-package-path-string ""))
    ;; See if `src' directory contain in the string.
    (when (jcs-contain-string jcs-java-source-dir-name tmp-file-name)

      ;; split the string by using string -> `src'.
      (setq tmp-split-dir-string (split-string tmp-file-name jcs-java-source-dir-name))

      ;; get the length of the split string
      (setq tmp-split-dir-string-size (length tmp-split-dir-string))

      ;; NOTE(jenchieh): Get the last part of the path.
      ;; If there are two `src' directory in the path.
      ;; We will just like to get the last part of the file path
      ;; and ignore whatever infront of them.
      (setq tmp-insert-package-path-string
            ;; Get the last part of the file path array.
            (nth (1- tmp-split-dir-string-size) tmp-split-dir-string))

      ;; Now we remove the file name from last part of path..
      (setq tmp-insert-package-path-string
            (jcs-remove-string-by-substring tmp-insert-package-path-string
                                            (jcs-get-file-name)))

      ;; NOTE(jenchieh): If the string is less than two.
      ;; Meaning the file is directly under `default package'/`src'
      ;; directory.
      (when (<= 2 (length tmp-insert-package-path-string))
        ;; Replace all `/' to `.'.
        (setq tmp-insert-package-path-string (jcs-replace-string
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

;;;###autoload
(defun jcs-java-insert-package-src ()
  "Insert the package source declaration."
  (interactive)
  (save-excursion
    ;; Goto the beginning of the buffer.
    (goto-char (point-min))

    ;; Check if the first word package.
    (when (jcs-current-word-equal-p "package")
      ;; kill the old package source declaration.
      (jcs-kill-whole-line))

    ;; insert the new one.
    (jcs-java-insert-package-from-src)

    (jcs-keep-one-line-between)))


(require 'organize-imports-java)

;; NOTE(jenchieh): Set the source control list to the
;; same as mine source control list.
(setq organize-imports-java-vc-list jcs-vc-list)

;;;###autoload
(defun jcs-java-organize-imports ()
  "Organize all the import package.
Including adding or removing the package path."
  (interactive)
  ;; first organize package declaration.
  (jcs-java-insert-package-src)

  ;; Organize all the imports.
  (organize-imports-java-do-imports))

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defvar jcs-java-font-lock-type-face-missing-modes '(java-mode
                                                     jdee-mode)
  "Modes to fixed variable font lock missing face for Java.")

(mapc (lambda (mode)
        (let ((case-fold-search t))
          (font-lock-add-keywords
           mode
           '(("^[ ]*\\([A-Z][a-zA-Z0-9_-]*\\)[a-zA-Z0-9._-]*\\.[a-zA-Z0-9_-]*[(]" 1 'font-lock-type-face t)
             ("\\([A-Z][a-zA-Z0-9._-]*\\)\\.[a-zA-Z0-9_-]*[),:]" 1 'font-lock-type-face t)
             )'end)))
      jcs-java-font-lock-type-face-missing-modes)
