;;; jcs-template.el --- Template format  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "File Header Insertion" )
;;

(defun jcs-insert-header-if-empty (insert-func &optional ci)
  "Execute INSERT-FUNC if empty, CI means `call-interactively'."
  (when (jcs-current-file-empty-p)
    (if ci (call-interactively insert-func) (funcall insert-func))
    (goto-char (point-min))))

(cl-defun jcs-insert-header-if-valid (reg-lst insert-func &key interactive success fail)
  "Insert the header if certain conditions met.

REG-LST is extension list represent by regular expression.
INSERT-FUNC is the function that will be use to call inserting header content.
INTERACTIVE is boolean check if called function interactively instead.
SUCCESS is callback after successfully inserted header content.
FAILED is callback if does NOT successfully inserted header content."
  (require 'f)
  (jcs-reload-file-info)
  (let (result)
    (when (and buffer-file-name
               (not (file-exists-p buffer-file-name))
               (jcs-contain-list-type-str (f-filename buffer-file-name) reg-lst 'regex))
      (setq result (jcs-insert-header-if-empty insert-func interactive)))
    (if result
        (when (functionp success) (funcall success))
      (when (functionp fail) (funcall fail)))
    result))

;;
;; (@* "Buffer String" )
;;

(defvar jcs-template--header-double-colon nil
  "Preload the double colon file info template.")

(defvar jcs-template--header-double-dash nil
  "Preload the double dash file info template.")

(defvar jcs-template--header-double-quote nil
  "Preload the double quote file info template.")

(defvar jcs-template--header-double-semicolon nil
  "Preload the double semicolon file info template.")

(defvar jcs-template--header-double-slash nil
  "Preload the double slash file info template.")

(defvar jcs-template--header-triple-slash nil
  "Preload the triple slash file info template.")

(defvar jcs-template--header-global nil
  "Preload the global file info template.")

(defvar jcs-template--header-sharp nil
  "Preload the sharp file info template.")

(defvar jcs-template--header-semicolon nil
  "Preload the semicolon file info template.")

(defvar jcs-template--header-single-quote nil
  "Preload the single quote file info template.")

(defvar jcs-template--header-tag nil
  "Preload the tag file info template.")

(defvar jcs-template--headers-loaded-p nil
  "Return non-nil, if headers are loaded as cache.")


(defun jcs-reload-file-info (&optional force)
  "Reload the header templates once.

If optional argument FORCE is non-nil, refresh cache once."
  (interactive)
  (when (or force (null jcs-template--headers-loaded-p))
    (setq jcs-template--header-double-colon (file-header-template-string "__header/d_colon.txt")
          jcs-template--header-double-dash (file-header-template-string "__header/d_dash.txt")
          jcs-template--header-double-quote (file-header-template-string "__header/d_quote.txt")
          jcs-template--header-double-semicolon (file-header-template-string "__header/d_semicolon.txt")
          jcs-template--header-double-slash (file-header-template-string "__header/d_slash.txt")
          jcs-template--header-triple-slash (file-header-template-string "__header/t_slash.txt")
          jcs-template--header-global (file-header-template-string "__header/global.txt")
          jcs-template--header-semicolon (file-header-template-string "__header/semicolon.txt")
          jcs-template--header-sharp (file-header-template-string "__header/sharp.txt")
          jcs-template--header-single-quote (file-header-template-string "__header/singlequote.txt")
          jcs-template--header-tag (file-header-template-string "__header/tag.txt")
          jcs-template--headers-loaded-p t)))

;;
;; (@* "Header" )
;;

(defun jcs-template-header-double-colon ()
  "Return the preloaded double colon file info template."
  (file-header-swap-keyword-template jcs-template--header-double-colon))

(defun jcs-template-header-double-dash ()
  "Return the preloaded double dash file info template."
  (file-header-swap-keyword-template jcs-template--header-double-dash))

(defun jcs-template-header-double-quote ()
  "Return the preloaded double quote file info template."
  (file-header-swap-keyword-template jcs-template--header-double-quote))

(defun jcs-template-header-double-semicolon ()
  "Return the preloaded double semicolon file info template."
  (file-header-swap-keyword-template jcs-template--header-double-semicolon))

(defun jcs-template-header-double-slash ()
  "Return the preloaded double slash file info template."
  (file-header-swap-keyword-template jcs-template--header-double-slash))

(defun jcs-template-header-triple-slash ()
  "Return the preloaded triple slash file info template."
  (file-header-swap-keyword-template jcs-template--header-triple-slash))

(defun jcs-template-header-global ()
  "Return the preloaded global file info template."
  (file-header-swap-keyword-template jcs-template--header-global))

(defun jcs-template-header-semicolon ()
  "Return the preloaded semicolon file info template."
  (file-header-swap-keyword-template jcs-template--header-semicolon))

(defun jcs-template-header-sharp ()
  "Return the preloaded sharp file info template."
  (file-header-swap-keyword-template jcs-template--header-sharp))

(defun jcs-template-header-single-quote ()
  "Return the preloaded single quote file info template."
  (file-header-swap-keyword-template jcs-template--header-single-quote))

(defun jcs-template-header-tag ()
  "Return the preloaded tag file info template."
  (file-header-swap-keyword-template jcs-template--header-tag))

(provide 'jcs-template)
;;; jcs-template.el ends here
