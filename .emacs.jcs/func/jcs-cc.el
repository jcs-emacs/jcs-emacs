;;; jcs-cc.el --- C/C++ related.  -*- lexical-binding: t -*-
;;; Commentary: Functions for C/C++ common.
;;; Code:

(require 'cl-lib)

;;;###autoload
(defun jcs-toggle-cc-mode ()
  "Toggle c/c++ mode."
  (interactive)
  (cl-case major-mode (c-mode (c++-mode)) (c++mode (c-mode))))

;;;###autoload
(defun jcs-toggle-c-comment-style ()
  "Toggle comment style between /* */ and //."
  (interactive)
  (when (or (jcs-is-current-major-mode-p "c-mode")
            (jcs-is-current-major-mode-p "c++-mode"))
    (if (string= comment-start "// ")
        (setq comment-start "/*"
              comment-start-skip "/\\*+[ \t]*"
              comment-end "*/"
              comment-end-skip "[ \t]*\\*+/")
      (setq comment-start "// "
            comment-end ""))))

;;
;; (@* "Document String" )
;;

(defvar jcs--class-desc-string "Class description here.."
  "Class description string.")
(defvar jcs--struct-desc-string "Struct description here.."
  "Struct description string.")
(defvar jcs--define-desc-string "Define description here.."
  "Define description string.")
(defvar jcs--enum-desc-string "Enum description here.."
  "Enum description string.")

(defun jcs-c++--docstr-before (search-string)
  "Local hook `docstr-before-insert-hook' for C++."
  (let ((splitted-search-string (split-string search-string " " t))
        (defined-keyword ""))
    (cond
     ((string-match-p "class" search-string)
      ;; Process class tag.
      (insert "@class ")
      (setq defined-keyword
            (jcs-find-item-in-list-offset splitted-search-string "[:{]" -1))
      (unless defined-keyword
        (setq defined-keyword (jcs-last-item-in-list splitted-search-string)))
      (ignore-errors (insert defined-keyword))
      (indent-for-tab-command)

      ;; Process brief tag.
      (insert "\n* @brief ")
      (insert jcs--class-desc-string)
      (indent-for-tab-command))
     ((string-match-p "struct" search-string)
      ;; Process class tag.
      (insert "@struct ")
      (setq defined-keyword
            (jcs-find-item-in-list-offset splitted-search-string "[:{]" -1))
      (unless defined-keyword
        (setq defined-keyword (jcs-last-item-in-list splitted-search-string)))
      (ignore-errors (insert defined-keyword))
      (indent-for-tab-command)

      ;; Process brief tag.
      (insert "\n* @brief ")
      (insert jcs--struct-desc-string)
      (indent-for-tab-command))
     ((or (string-match-p "define" search-string)
          (string-match-p "#define" search-string))
      ;; Process define tag.
      (insert "@def ")
      (setq defined-keyword (nth 1 (split-string search-string " " t)))
      (ignore-errors (insert defined-keyword))
      (indent-for-tab-command)

      ;; Process brief tag.
      (insert "\n* @brief ")
      (insert jcs--define-desc-string)
      (indent-for-tab-command))
     ((string-match-p "enum" search-string)
      ;; Process enumerator tag.
      (insert "@enum ")
      (setq defined-keyword
            (jcs-find-item-in-list-offset splitted-search-string "[:{]" -1))
      (unless defined-keyword
        (setq defined-keyword (jcs-last-item-in-list splitted-search-string)))
      (ignore-errors (insert defined-keyword))
      (indent-for-tab-command)

      ;; Process brief tag.
      (insert "\n* @brief ")
      (insert jcs--enum-desc-string)
      (indent-for-tab-command)))))

(provide 'jcs-cc)
;;; jcs-cc.el ends here
