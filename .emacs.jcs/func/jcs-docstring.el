;;; jcs-docstring.el --- Docstring related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Core" )
;;

(defconst jcs-docstring-modes
  '(actionscript-mode
    cc-mode c-mode c++-mode
    csharp-mode
    go-mode
    groovy-mode
    java-mode
    jayces-mode
    javascript-mode js-mode js2-mode js3-mode
    lua-mode
    masm-mode nasm-mode
    php-mode
    python-mode
    rjsx-mode
    rust-mode
    scala-mode
    typescript-mode
    web-mode)
  "List of major modes that supports docstring.")


(defun jcs-docstring-modes-p ()
  "Check if current mode support docstring."
  (jcs-is-current-major-mode-p jcs-docstring-modes))

(defun jcs-docstring-reload-faces ()
  "Reload the faces once."
  (dolist (mode jcs-docstring-modes)
    (font-lock-add-keywords
     mode
     '(;; `@param` { typename } val-tag : value tag description..
       ("\\(?:^\\|\\s-\\)\\(@[^ \"'{}()\t\r\n]+\\)" 1 'jcs-docstring-tag-face t)
       ;; @param `{ typename }` val-tag : value tag description..
       ("[ \t]+@[^ \t\r\n]+\\(?:^\\|\\s-\\)\\([\\[{][^}]*.\\)" 1 'jcs-docstring-type-face t)
       ;; @param { typename } `val-tag` : value tag description..
       ("[ \t]+@[^ \t\r\n].*[\]\|}]\\([^\r\n]*\\)[:-]" 1 'jcs-docstring-value-face t)
       ;; @param `val-tag` : value tag description..
       ("[ \t]+@[^ \t\r\n]*[ \t]*\\([a-zA-Z0-9_.*&]*\\)[ \t\n]*[{:-]" 1 'jcs-docstring-value-face t))
     'end)))

;;
;; (@* "Document String" )
;;

(defvar jcs--py-doc-string-version 0
  "Document string version.

0 : Description after \"\"\" opening docstring..
1 : Line break description \"\"\" opening docstring.")

;; All Languages
(defvar jcs--class-desc-string "" "Class description string.")
(defvar jcs--struct-desc-string "" "Struct description string.")
(defvar jcs--define-desc-string "" "Define description string.")
(defvar jcs--enum-desc-string "" "Enum description string.")
(defvar jcs--param-desc-string "" "Param description string.")
(defvar jcs--return-desc-string "" "Return description string.")

(defvar jcs--default-typename-string "" "Return default type name string.")

;;; Doc string character after value type font.
(defvar jcs--as-doc--after-value-type-char ""
  "Character after value type been inserted in ActionScript Mode.")
(defvar jcs--cc-doc--after-value-type-char ""
  "Character after value type been inserted in C/C++ Mode.")
(defvar jcs--csharp-doc--after-value-type-char ""
  "Character after value type been inserted in CSharp Mode.")
(defvar jcs--go-doc--after-value-type-char ""
  "Character after value type been inserted in Go Mode.")
(defvar jcs--groovy-doc--after-value-type-char ""
  "Character after value type been inserted in Groovy Mode.")
(defvar jcs--java-doc--after-value-type-char ""
  "Character after value type been inserted in Java Mode.")
(defvar jcs--js-doc--after-value-type-char ""
  "Character after value type been inserted in JavaScript Mode.")
(defvar jcs--lua-doc--after-value-type-char ""
  "Character after value type been inserted in Lua Mode.")
(defvar jcs--py-doc--after-value-type-char ""
  "Character after value type been inserted in Python Mode.")
(defvar jcs--php-doc--after-value-type-char ""
  "Character after value type been inserted in PHP Mode.")
(defvar jcs--rust-doc--after-value-type-char ""
  "Character after value type been inserted in Rust Mode.")
(defvar jcs--scala-doc--after-value-type-char ""
  "Character after value type been inserted in Scala Mode.")
(defvar jcs--ts-doc--after-value-type-char ""
  "Character after value type been inserted in TypeScript Mode.")

;;; Show typename.
(defvar jcs--as-doc--show-typename nil
  "Show the typename betweeen the open and close charachter in ActionScript mode.")
(defvar jcs--cc-doc--show-typename nil
  "Show the typename betweeen the open and close charachter in C/C++ mode.")
(defvar jcs--csharp-doc--show-typename nil
  "Show the typename betweeen the open and close charachter in CSharp mode.")
(defvar jcs--go-doc--show-typename nil
  "Show the typename betweeen the open and close charachter in Go mode.")
(defvar jcs--groovy-doc--show-typename nil
  "Show the typename betweeen the open and close charachter in Groovy mode.")
(defvar jcs--java-doc--show-typename nil
  "Show the typename betweeen the open and close charachter in Java mode.")
(defvar jcs--js-doc--show-typename nil
  "Show the typename betweeen the open and close charachter in JavaScript mode.")
(defvar jcs--lua-doc--show-typename nil
  "Show the typename betweeen the open and close charachter in Lua mode.")
(defvar jcs--py-doc--show-typename nil
  "Show the typename betweeen the open and close charachter in Python mode.")
(defvar jcs--php-doc--show-typename nil
  "Show the typename betweeen the open and close charachter in PHP mode.")
(defvar jcs--scala-doc--show-typename nil
  "Show the typename betweeen the open and close charachter in Scala mode.")
(defvar jcs--rust-doc--show-typename nil
  "Show the typename betweeen the open and close charachter in Rust mode.")
(defvar jcs--ts-doc--show-typename nil
  "Show the typename betweeen the open and close charachter in TypeScript mode.")


;;; Tag strings
(defvar jcs--as--param-string "" "Parameter string in ActionScript mode.")
(defvar jcs--as--return-string "" "Returns string in ActionScript mode.")

(defvar jcs--cc--param-string "" "Parameter string in C/C++ mode.")
(defvar jcs--cc--return-string "" "Returns string in C/C++ mode.")

(defvar jcs--csharp--param-string "" "Parameter string in CSharp mode.")
(defvar jcs--csharp--return-string "" "Returns string in CSharp mode.")

(defvar jcs--go--param-string "" "Parameter string in Go mode.")
(defvar jcs--go--return-string "" "Returns string in Go mode.")

(defvar jcs--groovy--param-string "" "Parameter string in Groovy mode.")
(defvar jcs--groovy--return-string "" "Returns string in Groovy mode.")

(defvar jcs--java--param-string "" "Parameter string in Java mode.")
(defvar jcs--java--return-string "" "Returns string in Java mode.")

(defvar jcs--js--param-string "" "Parameter string in JavaScript mode.")
(defvar jcs--js--return-string "" "Returns string in JavaScript mode.")

(defvar jcs--lua--param-string "" "Parameter string in Lua mode.")
(defvar jcs--lua--return-string "" "Returns string in Lua mode.")

(defvar jcs--py--param-string "" "Parameter string in Pyhon mode.")
(defvar jcs--py--return-string "" "Returns string in Python mode.")

(defvar jcs--php--param-string "" "Parameter string in PHP mode.")
(defvar jcs--php--return-string "" "Returns string in PHP mode.")

(defvar jcs--rust--param-string "" "Parameter string in Rust mode.")
(defvar jcs--rust--return-string "" "Returns string in Rust mode.")

(defvar jcs--scala--param-string "" "Parameter string in Scala mode.")
(defvar jcs--scala--return-string "" "Returns string in Scala mode.")

(defvar jcs--ts--param-string "" "Parameter string in TypeScript mode.")
(defvar jcs--ts--return-string "" "Returns string in TypeScript mode.")


;;; Brackets
(defvar jcs--as--open-type-char "" "Character before the typename in ActionScript mode.")
(defvar jcs--as--close-type-char "" "Character after the typename in ActionScript mode.")

(defvar jcs--cc--open-type-char "" "Character before the typename in C/C++ mode.")
(defvar jcs--cc--close-type-char "" "Character after the typename in C/C++ mode.")

(defvar jcs--csharp--open-type-char "" "Character before the typename in CSharp mode.")
(defvar jcs--csharp--close-type-char "" "Character after the typename in CSharp mode.")

(defvar jcs--go--open-type-char "" "Character before the typename in Go mode.")
(defvar jcs--go--close-type-char "" "Character after the typename in Go mode.")

(defvar jcs--groovy--open-type-char "" "Character before the typename in Groovy mode.")
(defvar jcs--groovy--close-type-char "" "Character after the typename in Groovy mode.")

(defvar jcs--java--open-type-char "" "Character before the typename in Java mode.")
(defvar jcs--java--close-type-char "" "Character after the typename in Java mode.")

(defvar jcs--js--open-type-char "" "Character before the typename in JavaScript mode.")
(defvar jcs--js--close-type-char "" "Character after the typename in JavaScript mode.")

(defvar jcs--lua--open-type-char "" "Character before the typename in Lua mode.")
(defvar jcs--lua--close-type-char "" "Character after the typename in Lua mode.")

(defvar jcs--py--open-type-char "" "Character before the typename in Python mode.")
(defvar jcs--py--close-type-char "" "Character after the typename in Python mode.")

(defvar jcs--php--open-type-char "" "Character before the typename in PHP mode.")
(defvar jcs--php--close-type-char "" "Character after the typename in PHP mode.")

(defvar jcs--rust--open-type-char "" "Character before the typename in Rust mode.")
(defvar jcs--rust--close-type-char "" "Character after the typename in Rust mode.")

(defvar jcs--scala--open-type-char "" "Character before the typename in Scala mode.")
(defvar jcs--scala--close-type-char "" "Character after the typename in Scala mode.")

(defvar jcs--ts--open-type-char "" "Character before the typename in TypeScript mode.")
(defvar jcs--ts--close-type-char "" "Character after the typename in TypeScript mode.")



(defconst jcs--docstring-config-filepath
  (expand-file-name "~/.emacs.jcs/docstring/docstring_config.properties")
  "Doc-string properties file.")


;;;###autoload
(defun jcs-reload-docstring-info ()
  "Reload the doc-string info once."
  (interactive)
  (let ((tmp-ini-list '()))
    ;; Read the doc-string configuration file.
    (setq tmp-ini-list (jcs-parse-ini jcs--docstring-config-filepath))

    ;; descriptions
    (setq jcs--class-desc-string (jcs-get-properties tmp-ini-list "CLASS_DESC_STRING")
          jcs--struct-desc-string (jcs-get-properties tmp-ini-list "STRUCT_DESC_STRING")
          jcs--define-desc-string (jcs-get-properties tmp-ini-list "DEFINE_DESC_STRING")
          jcs--enum-desc-string (jcs-get-properties tmp-ini-list "ENUM_DESC_STRING")
          jcs--param-desc-string (jcs-get-properties tmp-ini-list "PARAM_DESC_STRING")
          jcs--return-desc-string (jcs-get-properties tmp-ini-list "RETURN_DESC_STRING")
          jcs--default-typename-string (jcs-get-properties tmp-ini-list "DEFAULT_TYPENAME_STRING"))

    ;; show type name
    (setq jcs--as-doc--show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "AS_DOC_SHOW_TYPENAME"))
          jcs--cc-doc--show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "CC_DOC_SHOW_TYPENAME"))
          jcs--csharp-doc--show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "CSHARP_DOC_SHOW_TYPENAME"))
          jcs--go-doc--show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "GO_DOC_SHOW_TYPENAME"))
          jcs--groovy-doc--show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "GROOVY_DOC_SHOW_TYPENAME"))
          jcs--java-doc--show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "JAVA_DOC_SHOW_TYPENAME"))
          jcs--js-doc--show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "JS_DOC_SHOW_TYPENAME"))
          jcs--lua-doc--show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "LUA_DOC_SHOW_TYPENAME"))
          jcs--py-doc--show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "PY_DOC_SHOW_TYPENAME"))
          jcs--php-doc--show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "PHP_DOC_SHOW_TYPENAME"))
          jcs--rust-doc--show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "RUST_DOC_SHOW_TYPENAME"))
          jcs--scala-doc--show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "SCALA_DOC_SHOW_TYPENAME"))
          jcs--ts-doc--show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "TS_DOC_SHOW_TYPENAME")))

    ;; After value type character.
    (setq jcs--as-doc--after-value-type-char (jcs-get-properties tmp-ini-list "AS_AFTER_VALUE_TYPE")
          jcs--cc-doc--after-value-type-char (jcs-get-properties tmp-ini-list "CC_AFTER_VALUE_TYPE")
          jcs--csharp-doc--after-value-type-char (jcs-get-properties tmp-ini-list "CSHARP_AFTER_VALUE_TYPE")
          jcs--go-doc--after-value-type-char (jcs-get-properties tmp-ini-list "GO_AFTER_VALUE_TYPE")
          jcs--groovy-doc--after-value-type-char (jcs-get-properties tmp-ini-list "GROOVY_AFTER_VALUE_TYPE")
          jcs--java-doc--after-value-type-char (jcs-get-properties tmp-ini-list "JAVA_AFTER_VALUE_TYPE")
          jcs--js-doc--after-value-type-char (jcs-get-properties tmp-ini-list "JS_AFTER_VALUE_TYPE")
          jcs--lua-doc--after-value-type-char (jcs-get-properties tmp-ini-list "LUA_AFTER_VALUE_TYPE")
          jcs--py-doc--after-value-type-char (jcs-get-properties tmp-ini-list "PY_AFTER_VALUE_TYPE")
          jcs--php-doc--after-value-type-char (jcs-get-properties tmp-ini-list "PHP_AFTER_VALUE_TYPE")
          jcs--rust-doc--after-value-type-char (jcs-get-properties tmp-ini-list "RUST_AFTER_VALUE_TYPE")
          jcs--scala-doc--after-value-type-char (jcs-get-properties tmp-ini-list "SCALA_AFTER_VALUE_TYPE")
          jcs--ts-doc--after-value-type-char (jcs-get-properties tmp-ini-list "TS_AFTER_VALUE_TYPE"))

    ;; param string
    (setq jcs--as--param-string (jcs-get-properties tmp-ini-list "AS_PARAM_STRING")
          jcs--cc--param-string (jcs-get-properties tmp-ini-list "CC_PARAM_STRING")
          jcs--csharp--param-string (jcs-get-properties tmp-ini-list "CSHARP_PARAM_STRING")
          jcs--go--param-string (jcs-get-properties tmp-ini-list "GO_PARAM_STRING")
          jcs--groovy--param-string (jcs-get-properties tmp-ini-list "GROOVY_PARAM_STRING")
          jcs--java--param-string (jcs-get-properties tmp-ini-list "JAVA_PARAM_STRING")
          jcs--js--param-string (jcs-get-properties tmp-ini-list "JS_PARAM_STRING")
          jcs--lua--param-string (jcs-get-properties tmp-ini-list "LUA_PARAM_STRING")
          jcs--py--param-string (jcs-get-properties tmp-ini-list "PY_PARAM_STRING")
          jcs--php--param-string (jcs-get-properties tmp-ini-list "PHP_PARAM_STRING")
          jcs--rust--param-string (jcs-get-properties tmp-ini-list "RUST_PARAM_STRING")
          jcs--scala--param-string (jcs-get-properties tmp-ini-list "SCALA_PARAM_STRING")
          jcs--ts--param-string (jcs-get-properties tmp-ini-list "TS_PARAM_STRING"))

    ;; return string
    (setq jcs--as--return-string (jcs-get-properties tmp-ini-list "AS_RETURN_STRING")
          jcs--cc--return-string (jcs-get-properties tmp-ini-list "CC_RETURN_STRING")
          jcs--csharp--return-string (jcs-get-properties tmp-ini-list "CSHARP_RETURN_STRING")
          jcs--go--return-string (jcs-get-properties tmp-ini-list "GO_RETURN_STRING")
          jcs--groovy--return-string (jcs-get-properties tmp-ini-list "GROOVY_RETURN_STRING")
          jcs--java--return-string (jcs-get-properties tmp-ini-list "JAVA_RETURN_STRING")
          jcs--js--return-string (jcs-get-properties tmp-ini-list "JS_RETURN_STRING")
          jcs--lua--return-string (jcs-get-properties tmp-ini-list "LUA_RETURN_STRING")
          jcs--py--return-string (jcs-get-properties tmp-ini-list "PY_RETURN_STRING")
          jcs--php--return-string (jcs-get-properties tmp-ini-list "PHP_RETURN_STRING")
          jcs--rust--return-string (jcs-get-properties tmp-ini-list "RUST_RETURN_STRING")
          jcs--scala--return-string (jcs-get-properties tmp-ini-list "SCALA_RETURN_STRING")
          jcs--ts--return-string (jcs-get-properties tmp-ini-list "TS_RETURN_STRING"))

    ;; open type character.
    (setq jcs--as--open-type-char (jcs-get-properties tmp-ini-list "AS_OPEN_TYPE_CHAR")
          jcs--cc--open-type-char (jcs-get-properties tmp-ini-list "CC_OPEN_TYPE_CHAR")
          jcs--csharp--open-type-char (jcs-get-properties tmp-ini-list "CSHARP_OPEN_TYPE_CHAR")
          jcs--go--open-type-char (jcs-get-properties tmp-ini-list "GO_OPEN_TYPE_CHAR")
          jcs--groovy--open-type-char (jcs-get-properties tmp-ini-list "GROOVY_OPEN_TYPE_CHAR")
          jcs--java--open-type-char (jcs-get-properties tmp-ini-list "JAVA_OPEN_TYPE_CHAR")
          jcs--js--open-type-char (jcs-get-properties tmp-ini-list "JS_OPEN_TYPE_CHAR")
          jcs--lua--open-type-char (jcs-get-properties tmp-ini-list "LUA_OPEN_TYPE_CHAR")
          jcs--py--open-type-char (jcs-get-properties tmp-ini-list "PY_OPEN_TYPE_CHAR")
          jcs--php--open-type-char (jcs-get-properties tmp-ini-list "PHP_OPEN_TYPE_CHAR")
          jcs--rust--open-type-char (jcs-get-properties tmp-ini-list "RUST_OPEN_TYPE_CHAR")
          jcs--scala--open-type-char (jcs-get-properties tmp-ini-list "SCALA_OPEN_TYPE_CHAR")
          jcs--ts--open-type-char (jcs-get-properties tmp-ini-list "TS_OPEN_TYPE_CHAR"))

    ;; close type character.
    (setq jcs--as--close-type-char (jcs-get-properties tmp-ini-list "AS_CLOSE_TYPE_CHAR")
          jcs--cc--close-type-char (jcs-get-properties tmp-ini-list "CC_CLOSE_TYPE_CHAR")
          jcs--csharp--close-type-char (jcs-get-properties tmp-ini-list "CSHARP_CLOSE_TYPE_CHAR")
          jcs--go--close-type-char (jcs-get-properties tmp-ini-list "GO_CLOSE_TYPE_CHAR")
          jcs--groovy--close-type-char (jcs-get-properties tmp-ini-list "GROOVY_CLOSE_TYPE_CHAR")
          jcs--java--close-type-char (jcs-get-properties tmp-ini-list "JAVA_CLOSE_TYPE_CHAR")
          jcs--js--close-type-char (jcs-get-properties tmp-ini-list "JS_CLOSE_TYPE_CHAR")
          jcs--lua--close-type-char (jcs-get-properties tmp-ini-list "LUA_CLOSE_TYPE_CHAR")
          jcs--py--close-type-char (jcs-get-properties tmp-ini-list "PY_CLOSE_TYPE_CHAR")
          jcs--php--close-type-char (jcs-get-properties tmp-ini-list "PHP_CLOSE_TYPE_CHAR")
          jcs--rust--close-type-char (jcs-get-properties tmp-ini-list "RUST_CLOSE_TYPE_CHAR")
          jcs--scala--close-type-char (jcs-get-properties tmp-ini-list "SCALA_CLOSE_TYPE_CHAR")
          jcs--ts--close-type-char (jcs-get-properties tmp-ini-list "TS_CLOSE_TYPE_CHAR"))))


(defun jcs-insert-jsdoc-type (type-name open-char close-char)
  "Insert the curly bracket by this order => OPEN-CHAR, TYPE-NAME, CLOSE-CHAR."
  (insert open-char)
  (insert type-name)
  (insert close-char))


(defun jcs-insert-comment-style-by-current-line (sr-op)
  "Read the current line and insert by reading the need from the input line.
SR-OP is the boundary of the search limit."
  (require 's)
  (let ((search-string "") (close-bracket-pt -1))
    (save-excursion
      (jcs-move-to-forward-a-char ")")
      (setq close-bracket-pt (point)))

    (save-excursion
      (unless (jcs-current-line-empty-p)
        (let ((word-index 0) end-function-point)
          (save-excursion
            (save-window-excursion
              ;; NOTE: Find closing parenthesis instead of search for a line
              ;; will make this support multi-line doc-string.
              ;;
              ;; Goto beginning of line to prevent if we miss any closing
              ;; parenthesis before the point.
              (beginning-of-line)

              (let ((tmp-current-point (point)))
                ;; Move to next targeting end function character.
                (jcs-move-to-forward-a-char sr-op)

                ;; Check if we did move the point?
                ;; If the recorded point is the same as the current point, which
                ;; mean the cursor did not move at all.
                ;;
                ;; If that is the case, use the default one which is the new line.
                (when (or (= tmp-current-point (point))
                          (jcs-empty-line-between-point tmp-current-point (point)))
                  ;; back to where we were.
                  (goto-char tmp-current-point)
                  ;; Default to new line.
                  (jcs-move-to-forward-a-char "[\r\n]")))

              ;; After moved to the closing parenthesis, record down the point's
              ;; position.
              (setq end-function-point (1- (point)))))

          (beginning-of-line)

          ;; Get the search string after we found `end-function-point' and back
          ;; to searching point.
          (setq search-string (string-trim (buffer-substring (point) end-function-point)))
          ;; Replace line breaks to space.
          (setq search-string (s-replace "\n" " " search-string)))))

    ;; Insert document comment string.
    (jcs--insert-doc-comment-string search-string)))

(defconst jcs-docstring--writer-alist
  `(((actionscript-mode)                   . as)
    ((c-mode c++-mode)                     . cc)
    ((csharp-mode)                         . csharp)
    ((go-mode)                             . go)
    ((groovy-mode)                         . groovy)
    ((java-mode jdee-mode)                 . java)
    ((js-mode js2-mode js3-mode rjsx-mode) . js)
    ((lua-mode)                            . lua)
    ((php-mode web-mode)                   . php)
    ((rust-mode)                           . rust)
    ((scala-mode)                          . scala)
    ((typescript-mode)                     . ts))
  "List of writers according to each `major-mode'.")

(defun jcs-docstring--get-writer-key ()
  "Return writer from current `major-mode'."
  (require 'cl-lib)
  (let (key)
    (cl-some
     (lambda (modes)
       (when (jcs-is-current-major-mode-p (car modes))
         (setq key (cdr modes)))
       key)
     jcs-docstring--writer-alist)))

(defun jcs-docstring--form-writer (meet-function-name)
  "Form the writer function by MEET-FUNCTION-NAME."
  (let ((key (jcs-docstring--get-writer-key)) writer)
    (when key
      (setq writer
            (if meet-function-name
                (format "jcs--%s-mode-doc-string-func" key)
              (format "jcs--%s-mode-doc-string-others" key))))
    (if writer (intern writer) nil)))

(defun jcs--insert-doc-comment-string (search-string)
  "Insert document comment style.
SEARCH-STRING is the raw string that represent the code we want to document."
  (save-excursion
    (let* ((meet-function-name (jcs--function-name search-string))
           (mode-doc-str-fn (jcs-docstring--form-writer meet-function-name)))
      ;; NOTE: Ensure the `mode-doc-str-fn' is assign to something
      ;; valid to execute.
      (when mode-doc-str-fn
        (funcall mode-doc-str-fn search-string)))))


(defun jcs--function-name (search-string)
  "Analyze SEARCH-STRING to get function name."
  (let ((pos (jcs-last-regex-in-string "(" search-string)) fn-str)
    (when pos
      (setq fn-str (substring search-string 0 pos)
            fn-str (split-string fn-str " " t)
            fn-str (nth (1- (length fn-str)) fn-str)))
    (if (stringp fn-str) (string-trim fn-str) nil)))

(defun jcs--return-type (search-string)
  "Analyze SEARCH-STRING to get return type.
This is for c-like programming languages."
  (let ((pos (jcs-last-regex-in-string "(" search-string))
        return-type-str)
    (when pos
      (setq return-type-str (substring search-string 0 pos)
            return-type-str (split-string return-type-str " " t)
            return-type-str (nth (- (length return-type-str) 2) return-type-str)))
    (if (stringp return-type-str) (string-trim return-type-str) nil)))

(defun jcs--return-type-behind (search-string &optional spi-sym)
  "Analyze SEARCH-STRING to get return type.

This is for colon type programming languages.  For example, `actionscript',
`typescript', etc.

An optional argument SPI-SYM is the split symbol for return type."
  (let ((pos (jcs-last-regex-in-string ")" search-string))
        return-type-str)
    (when pos
      (setq return-type-str (substring search-string (1+ pos) (length search-string)))
      (when spi-sym
        (setq return-type-str (nth 1 (split-string return-type-str spi-sym)))))
    (if (and (stringp return-type-str)
             (not (string-empty-p return-type-str)))
        (string-trim return-type-str)
      nil)))

(defun jcs--analyze-param-string (search-string)
  "Get rid of the open and close parentheses, only get the center part.
SEARCH-STRING : string that use to analyze."
  (let (pos param-string)
    (setq param-string (substring search-string
                                  (1+ (jcs-last-regex-in-string "(" search-string))
                                  (length search-string))
          pos (jcs-last-regex-in-string ")" param-string)
          param-string (substring param-string 0 pos))
    param-string))

(defun jcs--param-empty-p (param-lst)
  "Check if the full PARAM-LST empty."
  (let ((param-lst-len (length param-lst))
        (index 0) (is-empty t))
    (while (and (< index param-lst-len) is-empty)
      (unless (string= "" (string-trim (nth index param-lst)))
        (setq is-empty nil))
      (setq index (1+ index)))
    is-empty))

(defun jcs-paren-param-list (search-string)
  "Return parentheses type parameter list using SEARCH-STRING.

This will works with programming language that define function like this

  `(type-name var-name, type-name var-name)`

or with default value

  `(type-name var-name = default-val, type-name var-name = default-val)`."
  (let ((param-string "") (param-lst '())
        (param-type-str-lst '()) (param-var-str-lst '())
        (param-type-strings nil) (param-variable-strings nil)
        (result-datas '()))
    (setq param-string (jcs--analyze-param-string search-string))

    (setq param-lst (split-string param-string ","))
    (when (jcs--param-empty-p param-lst)
      (setq param-lst '()))

    (dolist (param-sec-string param-lst)
      (let ((param-split-str-lst '())
            (param-split-str-lst-len -1) (param-split-str-lst-len-1 -1)
            (param-var-str "") (param-type-str ""))
        (setq param-sec-string (nth 0 (split-string param-sec-string "=")))
        (setq param-split-str-lst (jcs-chop param-sec-string " "))

        (delete-dups param-split-str-lst)
        (setq param-split-str-lst (remove " " param-split-str-lst))

        (setq param-split-str-lst-len (length param-split-str-lst))
        (setq param-split-str-lst-len-1 (1- param-split-str-lst-len))

        ;; Variable name should always be that last element in the list.
        (setq param-var-str (string-trim (nth param-split-str-lst-len-1 param-split-str-lst)))

        ;; Data type name should be the rest except the last element.
        (let ((index 0) (sep ""))
          (while (< index param-split-str-lst-len-1)
            (if (string= param-type-str "") (setq sep "") (setq sep " "))
            (setq param-type-str (concat param-type-str sep (string-trim (nth index param-split-str-lst))))
            (setq index (1+ index))))

        (unless (string= "" param-var-str)
          (push param-var-str param-var-str-lst))
        (unless (string= "" param-type-str)
          (push param-type-str param-type-str-lst))))

    (setq param-type-strings (reverse param-type-str-lst))
    (setq param-variable-strings (reverse param-var-str-lst))

    (push param-type-strings result-datas)
    (push param-variable-strings result-datas)

    (setq result-datas (reverse result-datas))
    result-datas))

(defun jcs-paren-param-list-behind (search-string &optional spi-sym last-word)
  "Like `jcs-paren-param-list' but handle programming languages that use colon \
to separate the type.

Support format like

  `(var-name : type-name, var-name : type-name)`

or with default value

  `(var-name : type-name = default-val, var-name : type-name = default-val)`.

See `jcs-paren-param-list' function for argument description SEARCH-STRING.

An optional argument SPI-SYM is the split symbol for return type.  In most cases,
this symbol often will be a 'colon'.

If optional argument LAST-WORD is non-nil; then limit the variable name to the
last word only."
  (let ((param-string "") (param-lst '())
        (param-type-str-lst '()) (param-var-str-lst '())
        (param-type-strings nil) (param-variable-strings nil)
        (result-datas '()))
    (setq param-string (jcs--analyze-param-string search-string))

    (setq param-lst (split-string param-string ","))
    (when (jcs--param-empty-p param-lst)
      (setq param-lst '()))

    (dolist (param-sec-string param-lst)
      (let ((param-split-str-lst '())
            (param-var-str "") (param-type-str ""))
        ;; First remove the possible default value.
        (setq param-sec-string (nth 0 (split-string param-sec-string "=[^>]")))
        (setq param-split-str-lst (split-string param-sec-string spi-sym))
        (setq param-var-str (string-trim (nth 0 param-split-str-lst)))
        (if (= (length param-split-str-lst) 1)
            ;; Set default type name string here.
            (setq param-type-str jcs--default-typename-string)
          (setq param-type-str (string-trim (nth 1 param-split-str-lst))))

        (when last-word
          (setq param-var-str (split-string param-var-str " " t)
                param-var-str (nth (1- (length param-var-str)) param-var-str)))

        (push param-var-str param-var-str-lst)
        (push param-type-str param-type-str-lst)))

    (setq param-type-strings (reverse param-type-str-lst))
    (setq param-variable-strings (reverse param-var-str-lst))

    (push param-type-strings result-datas)
    (push param-variable-strings result-datas)

    (setq result-datas (reverse result-datas))
    result-datas))

;;
;; (@* "ActionScript" )
;;

(defun jcs--as-mode-doc-string-others (search-string)
  "Insert `actionscript-mode' other doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (cond
   ((string-match-p "class" search-string)
    (progn
      ;; TODO: implement into ActionScript mode.
      ))))

(defun jcs--as-mode-doc-string-func (search-string)
  "Insert `actionscript-mode' function doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (let* ((paren-param-list (jcs-paren-param-list-behind search-string ":"))
         (param-type-strings (nth 0 paren-param-list))
         (param-variable-strings (nth 1 paren-param-list))
         (param-var-len (length param-variable-strings))
         (param-index 0)
         ;; Get all return data types.
         (return-type-str (jcs--return-type-behind search-string ":"))
         (there-is-return (not (null return-type-str))))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ; start from newline.
      (insert "* @")
      (insert jcs--as--param-string)
      (when jcs--as-doc--show-typename
        (jcs-insert-jsdoc-type (nth param-index param-type-strings)
                               jcs--as--open-type-char
                               jcs--as--close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs--as-doc--after-value-type-char)
      (insert jcs--param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-str "void")
        (insert "\n")
        (insert "* @")
        (insert jcs--as--return-string)
        (when jcs--as-doc--show-typename
          (jcs-insert-jsdoc-type return-type-str
                                 jcs--as--open-type-char
                                 jcs--as--close-type-char))
        (backward-delete-char 1)
        (if jcs--as-doc--show-typename
            (insert jcs--as-doc--after-value-type-char)
          (insert " "))
        (insert jcs--return-desc-string)
        (indent-for-tab-command)))))

;;
;; (@* "C/C++" )
;;

(defun jcs--cc-mode-doc-string-others (search-string)
  "Insert `c-mode' or `c++-mode' other doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (let ((splitted-search-string (split-string search-string " " t))
        (defined-keyword ""))
    (cond
     ((string-match-p "class" search-string)
      (progn
        ;; go back to comment line.
        (jcs-previous-line)
        (jcs-previous-line)
        (end-of-line)

        ;; Process class tag.
        (insert "@class ")
        (setq defined-keyword
              (jcs-find-item-in-list-offset splitted-search-string "[:{]" -1))
        (unless defined-keyword
          (setq defined-keyword (jcs-last-item-in-list splitted-search-string)))
        (ignore-errors (insert defined-keyword))
        (indent-for-tab-command)

        ;; Process brief tag.
        (insert "\n")
        (insert "* @brief ")
        (insert jcs--class-desc-string)
        (indent-for-tab-command)))
     ((string-match-p "struct" search-string)
      (progn
        ;; go back to comment line.
        (jcs-previous-line)
        (jcs-previous-line)
        (end-of-line)

        ;; Process class tag.
        (insert "@struct ")
        (setq defined-keyword
              (jcs-find-item-in-list-offset splitted-search-string "[:{]" -1))
        (unless defined-keyword
          (setq defined-keyword (jcs-last-item-in-list splitted-search-string)))
        (ignore-errors (insert defined-keyword))
        (indent-for-tab-command)

        ;; Process brief tag.
        (insert "\n")
        (insert "* @brief ")
        (insert jcs--struct-desc-string)
        (indent-for-tab-command)))
     ((or (string-match-p "define" search-string)
          (string-match-p "#define" search-string))
      (progn
        ;; go back to comment line.
        (jcs-previous-line)
        (jcs-previous-line)
        (end-of-line)

        ;; Process define tag.
        (insert "@def ")
        (setq defined-keyword (nth 1 (split-string search-string " " t)))
        (ignore-errors (insert defined-keyword))
        (indent-for-tab-command)

        ;; Process brief tag.
        (insert "\n")
        (insert "* @brief ")
        (insert jcs--define-desc-string)
        (indent-for-tab-command)))
     ((string-match-p "enum" search-string)
      (progn
        ;; go back to comment line.
        (jcs-previous-line)
        (jcs-previous-line)
        (end-of-line)

        ;; Process enumerator tag.
        (insert "@enum ")
        (setq defined-keyword
              (jcs-find-item-in-list-offset splitted-search-string "[:{]" -1))
        (unless defined-keyword
          (setq defined-keyword (jcs-last-item-in-list splitted-search-string)))
        (ignore-errors (insert defined-keyword))
        (indent-for-tab-command)

        ;; Process brief tag.
        (insert "\n")
        (insert "* @brief ")
        (insert jcs--enum-desc-string)
        (indent-for-tab-command))))))

(defun jcs--cc-mode-doc-string-func (search-string)
  "Insert `c-mode' or `c++-mode' function doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (let* ((paren-param-list (jcs-paren-param-list search-string))
         (param-type-strings (nth 0 paren-param-list))
         (param-variable-strings (nth 1 paren-param-list))
         (param-var-len (length param-variable-strings))
         (param-index 0)
         (fn-str (jcs--function-name search-string))
         ;; Get the return data type.
         (return-type-str (jcs--return-type search-string))
         (there-is-return (not (null return-type-str))))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process Function name.
    (insert "@func ")
    (insert fn-str)
    (indent-for-tab-command)

    ;; Process Breif description.
    (insert "\n")
    (insert "* @brief Function description here..")
    (indent-for-tab-command)

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ; start from newline.
      (insert "* @")
      (insert jcs--cc--param-string)
      (when jcs--cc-doc--show-typename
        (jcs-insert-jsdoc-type (nth param-index param-type-strings)
                               jcs--cc--open-type-char
                               jcs--cc--close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs--cc-doc--after-value-type-char)
      (insert jcs--param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-str "void")
        (insert "\n")
        (insert "* @")
        (insert jcs--cc--return-string)
        (if jcs--cc-doc--show-typename
            (jcs-insert-jsdoc-type return-type-str
                                   jcs--cc--open-type-char
                                   jcs--cc--close-type-char))
        (backward-delete-char 1)
        (if jcs--cc-doc--show-typename
            (insert jcs--cc-doc--after-value-type-char)
          (insert " "))
        (insert jcs--return-desc-string)
        (indent-for-tab-command)))))

;;
;; (@* "C#" )
;;

(defun jcs--csharp-mode-doc-string-others (search-string)
  "Insert `csharp-mode' other doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (cond
   ((string-match-p "class" search-string)
    (progn
      ;; STUDY: Don't think that C# doc need one..
      ))
   ((string-match-p "struct" search-string)
    (progn
      ;; STUDY: Don't think that C# doc need one..
      ))
   ((or (string-match-p "define" search-string)
        (string-match-p "#define" search-string))
    (progn
      ;; STUDY: Don't think that C# doc need one..
      ))))

(defun jcs--csharp-mode-doc-string-func (search-string)
  "Insert `csharp-mode' function doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (let* ((paren-param-list (jcs-paren-param-list search-string))
         (param-type-strings (nth 0 paren-param-list))
         (param-variable-strings (nth 1 paren-param-list))
         (param-var-len (length param-variable-strings))
         (param-index 0)
         (docstring-type -1)
         ;; Get the return data type.
         (return-type-str (jcs--return-type search-string))
         (there-is-return (not (null return-type-str))))
    ;; go back to comment line.
    (jcs-previous-line)
    (end-of-line)

    ;; Determine the docstring type.
    (save-excursion
      (backward-char 1)
      (cond ((jcs-current-char-equal-p "*") (setq docstring-type 1))
            (t (setq docstring-type 0))))

    (cond
     ((= docstring-type 0)
      (progn
        ;; First process param tag.
        (while (< param-index param-var-len)
          (insert "\n")  ; start from newline.
          (insert "/// <param name=\"")
          (insert (nth param-index param-variable-strings))
          (insert "\"></param>")

          ;; indent once.
          (indent-for-tab-command)

          ;; add up counter.
          (setq param-index (1+ param-index)))

        ;; Lastly, process returns tag.
        (when there-is-return
          (unless (string= return-type-str "void")
            (insert "\n")
            (insert "/// <returns></returns>")
            (indent-for-tab-command)))))
     ((= docstring-type 1)
      (progn
        ;; NOTE: This type of docstring, comment line is one more line above!
        (jcs-previous-line)
        (end-of-line)

        ;; Process param tag.
        (while (< param-index param-var-len)
          (insert "\n")  ; start from newline.
          (insert "* @")
          (insert jcs--as--param-string)
          (when jcs--as-doc--show-typename
            (jcs-insert-jsdoc-type (nth param-index param-type-strings)
                                   jcs--as--open-type-char
                                   jcs--as--close-type-char))
          (insert (nth param-index param-variable-strings))
          (insert jcs--as-doc--after-value-type-char)
          (insert jcs--param-desc-string)

          ;; indent once.
          (indent-for-tab-command)

          ;; add up counter.
          (setq param-index (1+ param-index)))

        ;; Lastly, process returns tag.
        (when there-is-return
          (unless (string= return-type-str "void")
            (insert "\n")
            (insert "* @")
            (insert jcs--as--return-string)
            (when jcs--as-doc--show-typename
              (jcs-insert-jsdoc-type return-type-str
                                     jcs--as--open-type-char
                                     jcs--as--close-type-char))
            (backward-delete-char 1)
            (if jcs--as-doc--show-typename
                (insert jcs--as-doc--after-value-type-char)
              (insert " "))
            (insert jcs--return-desc-string)
            (indent-for-tab-command))))))))

;;
;; (@* "Golang" )
;;

(defun jcs--go-mode-doc-string-others (search-string)
  "Insert `go-mode' other doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  ;; NOTE: I haven't really got deep enough with Go yet.
  )

(defun jcs--go-mode-doc-string-func (search-string)
  "Insert `go-mode' function doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (let* ((paren-param-list (jcs-paren-param-list-behind search-string))
         (param-type-strings (nth 0 paren-param-list))
         (param-variable-strings (nth 1 paren-param-list))
         (param-var-len (length param-variable-strings))
         (param-index 0)
         (docstring-type -1)
         ;; Get the return data type.
         (return-type-str (jcs--return-type-behind search-string))
         (there-is-return (not (null return-type-str))))
    ;; go back to comment line.
    (jcs-previous-line)
    (end-of-line)

    ;; Determine the docstring type.
    (save-excursion
      (backward-char 1)
      (cond ((jcs-current-char-equal-p "*") (setq docstring-type 1))
            (t (setq docstring-type 0))))

    (cond
     ((= docstring-type 0)
      (progn
        ;; go back to comment line.
        (jcs-next-line)
        (end-of-line)
        (insert " ")

        ;; Process param tag.
        (while (< param-index param-var-len)
          (insert "\n")  ; start from newline.
          (insert "// @")
          (insert jcs--go--param-string)
          (when jcs--go-doc--show-typename
            (jcs-insert-jsdoc-type (nth param-index param-type-strings)
                                   jcs--go--open-type-char
                                   jcs--go--close-type-char))
          (insert (nth param-index param-variable-strings))
          (insert jcs--go-doc--after-value-type-char)
          (insert jcs--param-desc-string)

          ;; indent once.
          (indent-for-tab-command)

          ;; add up counter.
          (setq param-index (1+ param-index)))

        ;; Lastly, process returns tag.
        (when there-is-return
          (unless (string= return-type-str "void")
            (insert "\n")
            (insert "// @")
            (insert jcs--go--return-string)
            (when jcs--go-doc--show-typename
              (jcs-insert-jsdoc-type return-type-str
                                     jcs--go--open-type-char
                                     jcs--go--close-type-char))
            (backward-delete-char 1)
            (if jcs--go-doc--show-typename
                (insert jcs--go-doc--after-value-type-char)
              (insert " "))
            (insert jcs--return-desc-string)
            (indent-for-tab-command)))))
     ((= docstring-type 1)
      (progn
        ;; go back to comment line.
        (jcs-previous-line)
        (end-of-line)

        ;; Process param tag.
        (while (< param-index param-var-len)
          (insert "\n")  ; start from newline.
          (insert "* @")
          (insert jcs--go--param-string)
          (when jcs--go-doc--show-typename
            (jcs-insert-jsdoc-type (nth param-index param-type-strings)
                                   jcs--go--open-type-char
                                   jcs--go--close-type-char))
          (insert (nth param-index param-variable-strings))
          (insert jcs--go-doc--after-value-type-char)
          (insert jcs--param-desc-string)

          ;; indent once.
          (indent-for-tab-command)

          ;; add up counter.
          (setq param-index (1+ param-index)))

        ;; Lastly, process returns tag.
        (when there-is-return
          (unless (string= return-type-str "void")
            (insert "\n")
            (insert "* @")
            (insert jcs--go--return-string)
            (when jcs--go-doc--show-typename
              (jcs-insert-jsdoc-type return-type-str
                                     jcs--go--open-type-char
                                     jcs--go--close-type-char))
            (backward-delete-char 1)
            (if jcs--go-doc--show-typename
                (insert jcs--go-doc--after-value-type-char)
              (insert " "))
            (insert jcs--return-desc-string)
            (indent-for-tab-command))))))))

;;
;; (@* "Groovy" )
;;

(defun jcs--groovy-mode-doc-string-others (search-string)
  "Insert `groovy-mode' other doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  ;; NOTE: I haven't really got deep enough with Groovy yet.
  )

(defun jcs--groovy-mode-doc-string-func (search-string)
  "Insert `groovy-mode' function doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (let* ((paren-param-list (jcs-paren-param-list search-string))
         (param-type-strings (nth 0 paren-param-list))
         (param-variable-strings (nth 1 paren-param-list))
         (param-var-len (length param-variable-strings))
         (param-index 0)
         ;; Get the return data type.
         (return-type-str (jcs--return-type search-string))
         (there-is-return (not (null return-type-str))))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ; start from newline.
      (insert "* @")
      (insert jcs--groovy--param-string)
      (when jcs--groovy-doc--show-typename
        (jcs-insert-jsdoc-type (nth param-index param-type-strings)
                               jcs--groovy--open-type-char
                               jcs--groovy--close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs--groovy-doc--after-value-type-char)
      (insert jcs--param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-str "void")
        (insert "\n")
        (insert "* @")
        (insert jcs--groovy--return-string)
        (when jcs--groovy-doc--show-typename
          (jcs-insert-jsdoc-type return-type-str
                                 jcs--groovy--open-type-char
                                 jcs--groovy--close-type-char))
        (backward-delete-char 1)
        (if jcs--groovy-doc--show-typename
            (insert jcs--groovy-doc--after-value-type-char)
          (insert " "))
        (insert jcs--return-desc-string)
        (indent-for-tab-command)))))

;;
;; (@* "Java" )
;;

(defun jcs--java-mode-doc-string-others (search-string)
  "Insert `java-mode' other doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (cond
   ((string-match-p "class" search-string)
    (progn
      ;; STUDY: Don't think that java doc need one..
      ))
   ((string-match-p "interface" search-string)
    (progn
      ;; STUDY: Don't think that java doc need one..
      ))))

(defun jcs--java-mode-doc-string-func (search-string)
  "Insert `java-mode' function doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (let* ((paren-param-list (jcs-paren-param-list search-string))
         (param-type-strings (nth 0 paren-param-list))
         (param-variable-strings (nth 1 paren-param-list))
         (param-var-len (length param-variable-strings))
         (param-index 0)
         ;; Get the return data type.
         (return-type-str (jcs--return-type search-string))
         (there-is-return (not (null return-type-str))))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ; start from newline.
      (insert "* @")
      (insert jcs--java--param-string)
      (when jcs--java-doc--show-typename
        (jcs-insert-jsdoc-type (nth param-index param-type-strings)
                               jcs--java--open-type-char
                               jcs--java--close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs--java-doc--after-value-type-char)
      (insert jcs--param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-str "void")
        (insert "\n")
        (insert "* @")
        (insert jcs--java--return-string)
        (when jcs--java-doc--show-typename
          (jcs-insert-jsdoc-type return-type-str
                                 jcs--java--open-type-char
                                 jcs--java--close-type-char))
        (backward-delete-char 1)
        (if jcs--java-doc--show-typename
            (insert jcs--java-doc--after-value-type-char)
          (insert " "))
        (insert jcs--return-desc-string)
        (indent-for-tab-command)))))

;;
;; (@* "JavaScript" )
;;

(defun jcs--js-mode-doc-string-others (search-string)
  "Insert `js2-mode' other doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (cond
   ((string-match-p "class" search-string)
    (progn
      ;; STUDY: Don't know if javascript need one..
      ))))

(defun jcs--js-mode-doc-string-func (search-string)
  "Insert `js2-mode' function doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (let* ((paren-param-list (jcs-paren-param-list search-string))
         (param-type-strings (nth 0 paren-param-list))
         (param-variable-strings (nth 1 paren-param-list))
         (param-var-len (length param-variable-strings))
         (param-index 0)
         ;; Get the return data type.
         (return-type-str "void")
         (there-is-return nil))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ; start from newline.
      (insert "* @")
      (insert jcs--js--param-string)
      (when jcs--js-doc--show-typename
        (jcs-insert-jsdoc-type jcs--default-typename-string
                               jcs--js--open-type-char
                               jcs--js--close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs--js-doc--after-value-type-char)
      (insert jcs--param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-str "void")
        (insert "\n")
        (insert "* @")
        (insert jcs--js--return-string)
        (when jcs--js-doc--show-typename
          (jcs-insert-jsdoc-type return-type-str
                                 jcs--js--open-type-char
                                 jcs--js--close-type-char))
        (backward-delete-char 1)
        (if jcs--js-doc--show-typename
            (insert jcs--js-doc--after-value-type-char)
          (insert " "))
        (insert jcs--return-desc-string)
        (indent-for-tab-command)))))

;;
;; (@* "Lua" )
;;

(defun jcs--lua-mode-doc-string-others (search-string)
  "Insert `lua-mode' other doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  ;; NOTE: I don't think Lua have any keywords...
  )

(defun jcs--lua-mode-doc-string-func (search-string)
  "Insert `lua-mode' function doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (let* ((paren-param-list (jcs-paren-param-list-behind search-string))
         (param-type-strings (nth 0 paren-param-list))
         (param-variable-strings (nth 1 paren-param-list))
         (param-var-len (length param-variable-strings))
         (param-index 0)
         ;; Get the return data type.
         (return-type-str "void")
         (there-is-return nil))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    (unless (= param-var-len 0)
      (insert (format "\n%s---" jcs-lua-doc-splitter)))

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ; start from newline.
      (insert "-- @")
      (insert jcs--lua--param-string)
      (when jcs--lua-doc--show-typename
        (jcs-insert-jsdoc-type jcs--default-typename-string
                               jcs--lua--open-type-char
                               jcs--lua--close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs--lua-doc--after-value-type-char)
      (insert jcs--param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-str "void")
        (insert "\n")
        (insert "-- @")
        (insert jcs--lua--return-string)
        (when jcs--lua-doc--show-typename
          (jcs-insert-jsdoc-type return-type-str
                                 jcs--lua--open-type-char
                                 jcs--lua--close-type-char))
        (backward-delete-char 1)
        (if jcs--lua-doc--show-typename
            (insert jcs--lua-doc--after-value-type-char)
          (insert " "))
        (insert jcs--return-desc-string)
        (indent-for-tab-command)))))

;;
;; (@* "Python" )
;;

(defun jcs--py-mode-doc-string-others (search-string)
  "Insert `python-mode' other doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (cond
   ((string-match-p "class" search-string)
    (progn
      ;; TODO: implement into python mode.
      ))))

(defun jcs--py-mode-doc-string-func (search-string)
  "Insert `python-mode' function doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (let* ((paren-param-list (jcs-paren-param-list-behind search-string))
         (param-type-strings (nth 0 paren-param-list))
         (param-variable-strings (nth 1 paren-param-list))
         (param-var-len (length param-variable-strings))
         (param-index 0)
         ;; Get the return data type.
         (return-type-str "void")
         (there-is-return nil))
    ;; go back to comment line.
    (jcs-move-to-forward-a-char-recursive "\"")
    (jcs-move-to-forward-a-char-recursive "\"")
    (jcs-move-to-forward-a-char-recursive "\"")

    ;; OPTION: docstring option..
    (when (= jcs--py-doc-string-version 1) (jcs-next-line))
    (end-of-line)

    ;; Line break between description and tags.
    (when (and (>= param-index 0)
               (not (= param-var-len 0)))
      (insert "\n"))

    (while (< param-index param-var-len)
      (unless (string= "self" (nth param-index param-variable-strings))
        (insert "\n")  ; start from newline.
        (insert "@")
        (insert jcs--py--param-string)
        (when jcs--py-doc--show-typename
          (jcs-insert-jsdoc-type jcs--default-typename-string
                                 jcs--py--open-type-char
                                 jcs--py--close-type-char))
        (insert (nth param-index param-variable-strings))
        (insert jcs--py-doc--after-value-type-char)
        (insert jcs--param-desc-string)

        ;; indent once.
        (indent-for-tab-command))

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-str "void")
        (insert "\n")
        (insert "@")
        (insert jcs--py--return-string)
        (when jcs--py-doc--show-typename
          (jcs-insert-jsdoc-type return-type-str
                                 jcs--py--open-type-char
                                 jcs--py--close-type-char))
        (backward-delete-char 1)
        (if jcs--py-doc--show-typename
            (insert jcs--py-doc--after-value-type-char)
          (insert " "))
        (insert jcs--return-desc-string)
        (indent-for-tab-command)))))

;;
;; (@* "PHP" )
;;

(defun jcs--php-mode-doc-string-others (search-string)
  "Insert `php-mode' other doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (cond
   ((string-match-p "class" search-string)
    (progn
      ;; TODO: implement into PHP mode.
      ))))

(defun jcs--php-mode-doc-string-func (search-string)
  "Insert `php-mode' function doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (let* ((paren-param-list (jcs-paren-param-list search-string))
         (param-type-strings (nth 0 paren-param-list))
         (param-variable-strings (nth 1 paren-param-list))
         (param-var-len (length param-variable-strings))
         (param-index 0)
         ;; Get the return data type.
         (return-type-str "void")
         (there-is-return nil))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ; start from newline.
      (insert "* @")
      (insert jcs--php--param-string)
      (when jcs--php-doc--show-typename
        (jcs-insert-jsdoc-type jcs--default-typename-string
                               jcs--php--open-type-char
                               jcs--php--close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs--php-doc--after-value-type-char)
      (insert jcs--param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-str "void")
        (insert "\n")
        (insert "* @")
        (insert jcs--php--return-string)
        (when jcs--php-doc--show-typename
          (jcs-insert-jsdoc-type return-type-str
                                 jcs--php--open-type-char
                                 jcs--php--close-type-char))
        (backward-delete-char 1)
        (if jcs--php-doc--show-typename
            (insert jcs--php-doc--after-value-type-char)
          (insert " "))
        (insert jcs--return-desc-string)
        (indent-for-tab-command)))))

;;
;; (@* "Rust" )
;;

(defun jcs--rust-mode-doc-string-others (search-string)
  "Insert `rust-mode' other doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (cond
   ((string-match-p "struct" search-string)
    (progn
      ;; TODO: implement into Rust mode.
      ))))

(defun jcs--rust-mode-doc-string-func (search-string)
  "Insert `rust-mode' function doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (let* ((paren-param-list (jcs-paren-param-list-behind search-string ":" t))
         (param-type-strings (nth 0 paren-param-list))
         (param-variable-strings (nth 1 paren-param-list))
         (param-var-len (length param-variable-strings))
         (param-type-len (length param-type-strings))
         (param-index 0)
         ;; Get all return data types.
         (return-type-str (jcs--return-type-behind search-string ":"))
         (there-is-return (not (null return-type-str))))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ; start from newline.
      (insert "* @")
      (insert jcs--rust--param-string)
      (when jcs--rust-doc--show-typename
        (jcs-insert-jsdoc-type (nth param-index param-type-strings)
                               jcs--rust--open-type-char
                               jcs--rust--close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs--rust-doc--after-value-type-char)
      (insert jcs--param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-str "void")
        (insert "\n")
        (insert "* @")
        (insert jcs--rust--return-string)
        (when jcs--rust-doc--show-typename
          (jcs-insert-jsdoc-type return-type-str
                                 jcs--rust--open-type-char
                                 jcs--rust--close-type-char))
        (backward-delete-char 1)
        (if jcs--rust-doc--show-typename
            (insert jcs--rust-doc--after-value-type-char)
          (insert " "))
        (insert jcs--return-desc-string)
        (indent-for-tab-command)))))

;;
;; (@* "Scala" )
;;

(defun jcs--scala-mode-doc-string-others (search-string)
  "Insert `scala-mode' other doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (cond
   ((string-match-p "class" search-string)
    (progn
      ;; TODO: implement into Scala mode.
      ))))

(defun jcs--scala-mode-doc-string-func (search-string)
  "Insert `scala-mode' function doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (let* ((paren-param-list (jcs-paren-param-list-behind search-string ":" t))
         (param-type-strings (nth 0 paren-param-list))
         (param-variable-strings (nth 1 paren-param-list))
         (param-var-len (length param-variable-strings))
         (param-type-len (length param-type-strings))
         (param-index 0)
         ;; Get all return data types.
         (return-type-str (jcs--return-type-behind search-string ":"))
         (there-is-return (not (null return-type-str))))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ; start from newline.
      (insert "* @")
      (insert jcs--scala--param-string)
      (when jcs--scala-doc--show-typename
        (jcs-insert-jsdoc-type (nth param-index param-type-strings)
                               jcs--scala--open-type-char
                               jcs--scala--close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs--scala-doc--after-value-type-char)
      (insert jcs--param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-str "void")
        (insert "\n")
        (insert "* @")
        (insert jcs--scala--return-string)
        (when jcs--scala-doc--show-typename
          (jcs-insert-jsdoc-type return-type-str
                                 jcs--scala--open-type-char
                                 jcs--scala--close-type-char))
        (backward-delete-char 1)
        (if jcs--scala-doc--show-typename
            (insert jcs--scala-doc--after-value-type-char)
          (insert " "))
        (insert jcs--return-desc-string)
        (indent-for-tab-command)))))

;;
;; (@* "TypeScript" )
;;

(defun jcs--ts-mode-doc-string-others (search-string)
  "Insert `typescript-mode' other doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (cond
   ((string-match-p "class" search-string)
    (progn
      ;; TODO: implement into TypeScript mode.
      ))))

(defun jcs--ts-mode-doc-string-func (search-string)
  "Insert `typescript-mode' function doc string.
SEARCH-STRING is the raw string that represent the code we want to document."
  (let* ((paren-param-list (jcs-paren-param-list-behind search-string ":"))
         (param-type-strings (nth 0 paren-param-list))
         (param-variable-strings (nth 1 paren-param-list))
         (param-var-len (length param-variable-strings))
         (param-type-len (length param-type-strings))
         (param-index 0)
         ;; Get all return data types.
         (return-type-str (jcs--return-type-behind search-string ":"))
         (there-is-return (not (null return-type-str))))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    (insert "@desc ")
    (indent-for-tab-command)

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ; start from newline.
      (insert "* @")
      (insert jcs--ts--param-string)
      (when jcs--ts-doc--show-typename
        (jcs-insert-jsdoc-type (nth param-index param-type-strings)
                               jcs--ts--open-type-char
                               jcs--ts--close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs--ts-doc--after-value-type-char)
      (insert jcs--param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-str "void")
        (insert "\n")
        (insert "* @")
        (insert jcs--ts--return-string)
        (when jcs--ts-doc--show-typename
          (jcs-insert-jsdoc-type return-type-str
                                 jcs--ts--open-type-char
                                 jcs--ts--close-type-char))
        (backward-delete-char 1)
        (if jcs--ts-doc--show-typename
            (insert jcs--ts-doc--after-value-type-char)
          (insert " "))
        (insert jcs--return-desc-string)
        (indent-for-tab-command)))))

(provide 'jcs-docstring)
;;; jcs-docstring.el ends here
