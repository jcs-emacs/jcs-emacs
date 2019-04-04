;;; jcs-oop-func.el --- OOP programming language related.
;;; Commentary: Functions for Object Oriented Programming languages.
;;; Code:


(defun jcs-docstring-modes-p ()
  "Check if current mode support docstring."
  (or (jcs-is-current-major-mode-p "actionscript-mode")
      (jcs-is-current-major-mode-p "c-mode")
      (jcs-is-current-major-mode-p "c++-mode")
      (jcs-is-current-major-mode-p "java-mode")
      ;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      ;; TODO(jenchieh): If we decide to use
      ;; c-type docstirng in `csharp-mode'. Then
      ;; we need to uncomment the line below.
      ;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      ;;(jcs-is-current-major-mode-p "csharp-mode")
      (jcs-is-current-major-mode-p "js2-mode")
      (jcs-is-current-major-mode-p "php-mode")
      (jcs-is-current-major-mode-p "typescript-mode")
      (jcs-is-current-major-mode-p "web-mode")))


(defvar jcs-oop-highlight-modes '(actionscript-mode
                                  cc-mode
                                  c-mode
                                  c++-mode
                                  csharp-mode
                                  java-mode
                                  jayces-mode
                                  js2-mode
                                  lua-mode
                                  nasm-mode
                                  php-mode
                                  python-mode
                                  ;;typescript-mode
                                  web-mode)
  "Modes to add OOP document comment style.")


;;;###autoload
(defun jcs-oop-reload-faces ()
  "Reload the faces once."
  (interactive)
  (mapc (lambda (mode)
          (font-lock-add-keywords
           mode
           '(("\\(?:^\\|\\s-\\)\\(@[a-zA-Z0-9_]*\\)" 1 'jcs-oop-tag-face t)
             ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
             ;; OPTION(jenchieh): Highlight curly bracket.
             ("@[a-zA-Z0-9_].*\\(?:^\\|\\s-\\)\\([\\[{].*.[\]}]\\)" 1 'jcs-oop-type-face t)
             ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
             ;; OR(jenchieh):
             ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
             ;; OPTION(jenchieh): Don't highlight curly bracket.
             ;;("@[a-zA-Z0-9_].*\\(?:^\\|\\s-\\)[\\[{]\\(.*.\\)[]}]" 1 'jcs-oop-type-face t)
             ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

             ;;
             ;; NOTE(jenchieh):
             ;; Doc String Style:
             ;;
             ;; @param { TypeName } `ValueTag' : value tag description..
             ;;
             ("@[a-zA-Z0-9_].*[\]\|}][ \t\n]*\\([a-zA-Z0-9_$*&]*\\)[ \t\n]*[:-]" 1 'jcs-oop-value-face t)
             ;;
             ;; NOTE(jenchieh):
             ;; Doc String Style:
             ;;
             ;; @param `ValueTag' : value tag description..
             ;;
             ("@[a-zA-Z0-9_]*[ \t\n]*\\([a-zA-Z0-9_.*&]*\\)[ \t\n]*[{:-]" 1 'jcs-oop-value-face t)
             )'end))
        jcs-oop-highlight-modes))


;;; Doc string
(defvar jcs-py-doc-string-version 0
  "Document string version.

0 : Description after \"\"\" opening docstring..
1 : Line breack description \"\"\" opening docstring.")


;; All Languages
(defvar jcs-class-desc-string "" "Class description string.")
(defvar jcs-struct-desc-string "" "Struct description string.")
(defvar jcs-define-desc-string "" "Define description string.")
(defvar jcs-enum-desc-string "" "Enum description string.")
(defvar jcs-param-desc-string "" "Param description string.")
(defvar jcs-return-desc-string "" "Return description string.")

(defvar jcs-default-typename-string "" "Return default type name string.")


;;; Doc string character after value type font.
(defvar jcs-as-doc-after-value-type-char ""
  "Character after value type been inserted in ActionScript Mode.")
(defvar jcs-cc-doc-after-value-type-char ""
  "Character after value type been inserted in C/C++ Mode.")
(defvar jcs-cs-doc-after-value-type-char ""
  "Character after value type been inserted in CSharp Mode.")
(defvar jcs-java-doc-after-value-type-char ""
  "Character after value type been inserted in Java Mode.")
(defvar jcs-js-doc-after-value-type-char ""
  "Character after value type been inserted in JavaScript Mode.")
(defvar jcs-lua-doc-after-value-type-char ""
  "Character after value type been inserted in Lua Mode.")
(defvar jcs-py-doc-after-value-type-char ""
  "Character after value type been inserted in Python Mode.")
(defvar jcs-php-doc-after-value-type-char ""
  "Character after value type been inserted in PHP Mode.")
(defvar jcs-ts-doc-after-value-type-char ""
  "Character after value type been inserted in TypeScript Mode.")


;;; Show typename.
(defvar jcs-as-doc-show-typename nil
  "Show the typename betweeen the open charachter and close charachter in ActionScript mode.")
(defvar jcs-cc-doc-show-typename nil
  "Show the typename betweeen the open charachter and close charachter in C/C++ mode.")
(defvar jcs-cs-doc-show-typename nil
  "Show the typename betweeen the open charachter and close charachter in CSharp mode.")
(defvar jcs-java-doc-show-typename nil
  "Show the typename betweeen the open charachter and close charachter in Java mode.")
(defvar jcs-js-doc-show-typename nil
  "Show the typename betweeen the open charachter and close charachter in JavaScript mode.")
(defvar jcs-lua-doc-show-typename nil
  "Show the typename betweeen the open charachter and close charachter in Lua mode.")
(defvar jcs-py-doc-show-typename nil
  "Show the typename betweeen the open charachter and close charachter in Python mode.")
(defvar jcs-php-doc-show-typename nil
  "Show the typename betweeen the open charachter and close charachter in PHP mode.")
(defvar jcs-ts-doc-show-typename nil
  "Show the typename betweeen the open charachter and close charachter in TypeScript mode.")


;;; Tag strings
(defvar jcs-as-param-string "" "Parameter string in ActionScript mode.")
(defvar jcs-as-return-string "" "Returns string in ActionScript mode.")

(defvar jcs-cc-param-string "" "Parameter string in C/C++ mode.")
(defvar jcs-cc-return-string "" "Returns string in C/C++ mode.")

(defvar jcs-cs-param-string "" "Parameter string in CSharp mode.")
(defvar jcs-cs-return-string "" "Returns string in CSharp mode.")

(defvar jcs-java-param-string "" "Parameter string in Java mode.")
(defvar jcs-java-return-string "" "Returns string in Java mode.")

(defvar jcs-js-param-string "" "Parameter string in JavaScript mode.")
(defvar jcs-js-return-string "" "Returns string in JavaScript mode.")

(defvar jcs-lua-param-string "" "Parameter string in Lua mode.")
(defvar jcs-lua-return-string "" "Returns string in Lua mode.")

(defvar jcs-py-param-string "" "Parameter string in Pyhon mode.")
(defvar jcs-py-return-string "" "Returns string in Python mode.")

(defvar jcs-php-param-string "" "Parameter string in PHP mode.")
(defvar jcs-php-return-string "" "Returns string in PHP mode.")

(defvar jcs-ts-param-string "" "Parameter string in TypeScript mode.")
(defvar jcs-ts-return-string "" "Returns string in TypeScript mode.")


;;; Brackets
(defvar jcs-as-open-type-char "" "Character before the typename in ActionScript mode.")
(defvar jcs-as-close-type-char "" "Character after the typename in ActionScript mode.")

(defvar jcs-cc-open-type-char "" "Character before the typename in C/C++ mode.")
(defvar jcs-cc-close-type-char "" "Character after the typename in C/C++ mode.")

(defvar jcs-cs-open-type-char "" "Character before the typename in CSharp mode.")
(defvar jcs-cs-close-type-char "" "Character after the typename in CSharp mode.")

(defvar jcs-java-open-type-char "" "Character before the typename in Java mode.")
(defvar jcs-java-close-type-char "" "Character after the typename in Java mode.")

(defvar jcs-js-open-type-char "" "Character before the typename in JavaScript mode.")
(defvar jcs-js-close-type-char "" "Character after the typename in JavaScript mode.")

(defvar jcs-lua-open-type-char "" "Character before the typename in Lua mode.")
(defvar jcs-lua-close-type-char "" "Character after the typename in Lua mode.")

(defvar jcs-py-open-type-char "" "Character before the typename in Python mode.")
(defvar jcs-py-close-type-char "" "Character after the typename in Python mode.")

(defvar jcs-php-open-type-char "" "Character before the typename in PHP mode.")
(defvar jcs-php-close-type-char "" "Character after the typename in PHP mode.")

(defvar jcs-ts-open-type-char "" "Character before the typename in TypeScript mode.")
(defvar jcs-ts-close-type-char "" "Character after the typename in TypeScript mode.")



(defvar jcs-docstring-config-filepath "~/.emacs.jcs/docstring/docstring_config.properties"
  "Doc-string properties file.")


;;;###autoload
(defun jcs-reload-docstring-info ()
  "Reload the doc-string info once."
  (interactive)

  (let ((tmp-ini-list '()))
    ;; Read the doc-string configuration file.
    (setq tmp-ini-list (jcs-parse-ini jcs-docstring-config-filepath))

    ;; descriptions
    (setq jcs-class-desc-string (jcs-get-properties tmp-ini-list "CLASS_DESC_STRING"))
    (setq jcs-struct-desc-string (jcs-get-properties tmp-ini-list "STRUCT_DESC_STRING"))
    (setq jcs-define-desc-string (jcs-get-properties tmp-ini-list "DEFINE_DESC_STRING"))
    (setq jcs-enum-desc-string (jcs-get-properties tmp-ini-list "ENUM_DESC_STRING"))
    (setq jcs-param-desc-string (jcs-get-properties tmp-ini-list "PARAM_DESC_STRING"))
    (setq jcs-return-desc-string (jcs-get-properties tmp-ini-list "RETURN_DESC_STRING"))
    (setq jcs-default-typename-string (jcs-get-properties tmp-ini-list "DEFAULT_TYPENAME_STRING"))

    ;; show type name
    (setq jcs-as-doc-show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "AS_DOC_SHOW_TYPENAME")))
    (setq jcs-cc-doc-show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "CC_DOC_SHOW_TYPENAME")))
    (setq jcs-cs-doc-show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "CS_DOC_SHOW_TYPENAME")))
    (setq jcs-java-doc-show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "JAVA_DOC_SHOW_TYPENAME")))
    (setq jcs-js-doc-show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "JS_DOC_SHOW_TYPENAME")))
    (setq jcs-lua-doc-show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "LUA_DOC_SHOW_TYPENAME")))
    (setq jcs-py-doc-show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "PY_DOC_SHOW_TYPENAME")))
    (setq jcs-php-doc-show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "PHP_DOC_SHOW_TYPENAME")))
    (setq jcs-ts-doc-show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "TS_DOC_SHOW_TYPENAME")))

    ;; After value type character.
    (setq jcs-as-doc-after-value-type-char (jcs-get-properties tmp-ini-list "AS_AFTER_VALUE_TYPE"))
    (setq jcs-cc-doc-after-value-type-char (jcs-get-properties tmp-ini-list "CC_AFTER_VALUE_TYPE"))
    (setq jcs-cs-doc-after-value-type-char (jcs-get-properties tmp-ini-list "CS_AFTER_VALUE_TYPE"))
    (setq jcs-java-doc-after-value-type-char (jcs-get-properties tmp-ini-list "JAVA_AFTER_VALUE_TYPE"))
    (setq jcs-js-doc-after-value-type-char (jcs-get-properties tmp-ini-list "JS_AFTER_VALUE_TYPE"))
    (setq jcs-lua-doc-after-value-type-char (jcs-get-properties tmp-ini-list "LUA_AFTER_VALUE_TYPE"))
    (setq jcs-py-doc-after-value-type-char (jcs-get-properties tmp-ini-list "PY_AFTER_VALUE_TYPE"))
    (setq jcs-php-doc-after-value-type-char (jcs-get-properties tmp-ini-list "PHP_AFTER_VALUE_TYPE"))
    (setq jcs-ts-doc-after-value-type-char (jcs-get-properties tmp-ini-list "TS_AFTER_VALUE_TYPE"))

    ;; param string
    (setq jcs-as-param-string (jcs-get-properties tmp-ini-list "AS_PARAM_STRING"))
    (setq jcs-cc-param-string (jcs-get-properties tmp-ini-list "CC_PARAM_STRING"))
    (setq jcs-cs-param-string (jcs-get-properties tmp-ini-list "CS_PARAM_STRING"))
    (setq jcs-java-param-string (jcs-get-properties tmp-ini-list "JAVA_PARAM_STRING"))
    (setq jcs-js-param-string (jcs-get-properties tmp-ini-list "JS_PARAM_STRING"))
    (setq jcs-lua-param-string (jcs-get-properties tmp-ini-list "LUA_PARAM_STRING"))
    (setq jcs-py-param-string (jcs-get-properties tmp-ini-list "PY_PARAM_STRING"))
    (setq jcs-php-param-string (jcs-get-properties tmp-ini-list "PHP_PARAM_STRING"))
    (setq jcs-ts-param-string (jcs-get-properties tmp-ini-list "TS_PARAM_STRING"))

    ;; return string
    (setq jcs-as-return-string (jcs-get-properties tmp-ini-list "AS_RETURN_STRING"))
    (setq jcs-cc-return-string (jcs-get-properties tmp-ini-list "CC_RETURN_STRING"))
    (setq jcs-cs-return-string (jcs-get-properties tmp-ini-list "CS_RETURN_STRING"))
    (setq jcs-java-return-string (jcs-get-properties tmp-ini-list "JAVA_RETURN_STRING"))
    (setq jcs-js-return-string (jcs-get-properties tmp-ini-list "JS_RETURN_STRING"))
    (setq jcs-lua-return-string (jcs-get-properties tmp-ini-list "LUA_RETURN_STRING"))
    (setq jcs-py-return-string (jcs-get-properties tmp-ini-list "PY_RETURN_STRING"))
    (setq jcs-php-return-string (jcs-get-properties tmp-ini-list "PHP_RETURN_STRING"))
    (setq jcs-ts-return-string (jcs-get-properties tmp-ini-list "TS_RETURN_STRING"))

    ;; open type character.
    (setq jcs-as-open-type-char (jcs-get-properties tmp-ini-list "AS_OPEN_TYPE_CHAR"))
    (setq jcs-cc-open-type-char (jcs-get-properties tmp-ini-list "CC_OPEN_TYPE_CHAR"))
    (setq jcs-cs-open-type-char (jcs-get-properties tmp-ini-list "CS_OPEN_TYPE_CHAR"))
    (setq jcs-java-open-type-char (jcs-get-properties tmp-ini-list "JAVA_OPEN_TYPE_CHAR"))
    (setq jcs-js-open-type-char (jcs-get-properties tmp-ini-list "JS_OPEN_TYPE_CHAR"))
    (setq jcs-lua-open-type-char (jcs-get-properties tmp-ini-list "LUA_OPEN_TYPE_CHAR"))
    (setq jcs-py-open-type-char (jcs-get-properties tmp-ini-list "PY_OPEN_TYPE_CHAR"))
    (setq jcs-php-open-type-char (jcs-get-properties tmp-ini-list "PHP_OPEN_TYPE_CHAR"))
    (setq jcs-ts-open-type-char (jcs-get-properties tmp-ini-list "TS_OPEN_TYPE_CHAR"))

    ;; close type character.
    (setq jcs-as-close-type-char (jcs-get-properties tmp-ini-list "AS_CLOSE_TYPE_CHAR"))
    (setq jcs-cc-close-type-char (jcs-get-properties tmp-ini-list "CC_CLOSE_TYPE_CHAR"))
    (setq jcs-cs-close-type-char (jcs-get-properties tmp-ini-list "CS_CLOSE_TYPE_CHAR"))
    (setq jcs-java-close-type-char (jcs-get-properties tmp-ini-list "JAVA_CLOSE_TYPE_CHAR"))
    (setq jcs-js-close-type-char (jcs-get-properties tmp-ini-list "JS_CLOSE_TYPE_CHAR"))
    (setq jcs-lua-close-type-char (jcs-get-properties tmp-ini-list "LUA_CLOSE_TYPE_CHAR"))
    (setq jcs-py-close-type-char (jcs-get-properties tmp-ini-list "PY_CLOSE_TYPE_CHAR"))
    (setq jcs-php-close-type-char (jcs-get-properties tmp-ini-list "PHP_CLOSE_TYPE_CHAR"))
    (setq jcs-ts-close-type-char (jcs-get-properties tmp-ini-list "TS_CLOSE_TYPE_CHAR"))
    ))


(defun jcs-insert-jsdoc-type (type-name open-char close-char)
  "Insert the curly bracket part.

TYPE-NAME : type name string.
OPEN-CHAR : opening character.
CLOSE-CHAR : closing character."
  (insert open-char)
  (insert type-name)
  (insert close-char))

(defun jcs-move-cursor-by-search-option (sr-op)
  "Move to next targeting end function character.
SR-OP :
0) search only current line.
1) search witch closing parenthesis.
2) search with opening culry parenthesis."
  (ignore-errors
    (cond ((= sr-op 0)
           ;; Only the current line.
           (end-of-line))
          ((= sr-op 1)
           ;; Closing Parenthesis
           ;; NOTE(jenchieh): No recursive/No prompt.
           (jcs-move-forward-close-paren t))
          ((= sr-op 2)
           ;; Opening Curly Parenthesis
           ;; NOTE(jenchieh): No recursive/No prompt.
           (jcs-move-forward-open-curlyParen t)))))


(defun jcs-insert-comment-style-by-current-line (sr-op)
  "Read the current line and insert by reading the need from \
the input line.

SR-OP :
0) search only current line.
1) search witch closing parenthesis.
2) search with opening culry parenthesis."

  (let ((keyword-strings '())
        (datatype-name "")
        (meet-function-name nil)
        (function-name-string "")
        (param-type-strings '())  ;; param type string list.
        (param-variable-strings '())  ;; param name string list.
        (there-is-return nil)
        (return-type-string "")
        (search-string "")
        (close-bracket-pt -1))

    (save-excursion
      (jcs-move-to-forward-a-char ")")
      (setq close-bracket-pt (point)))

    (save-excursion
      (when (not (jcs-current-line-empty-p))
        (let ((end-function-point nil)
              (word-index 0))

          (save-excursion
            (save-window-excursion
              ;; NOTE(jenchieh): Find closing parenthesis instead
              ;; of search for a line will make this support
              ;; multi-line doc-string.
              ;;
              ;; Goto beginning of line to prevent if we miss
              ;; any closing parenthesis before the point.
              (beginning-of-line)

              (let ((tmp-current-point (point)))
                ;; Move to next targeting end function character.
                (jcs-move-cursor-by-search-option sr-op)

                ;; Check if we did move the point?
                ;; If the recorded point is the same as
                ;; the current point, which mean the cursor
                ;; did not move at all.
                ;;
                ;; If that is the case, use the default one which
                ;; is the `end-of-line' function.
                (when (or (= tmp-current-point (point))
                          (jcs-empty-line-between-point tmp-current-point (point)))
                  ;; back to where we were.
                  (goto-char tmp-current-point)
                  ;; Use the default one. (Pass in zero)
                  (jcs-move-cursor-by-search-option 0)))

              ;; After moved to the closing parenthesis, record
              ;; down the point's position.
              (setq end-function-point (1- (point)))))

          (beginning-of-line)

          ;; Get the search string after we found `end-function-point' and
          ;; back to searching point.
          (setq search-string (string-trim (buffer-substring (point) end-function-point)))
          ;; Replace line breaks to space.
          (setq search-string (s-replace "\n" " " search-string))


          (while (< (point) end-function-point)
            (unless (= word-index 0)
              (forward-word 1))
            (forward-word 1)
            (backward-char 1)
            (setq word-index (1+ word-index))

            ;; Make sure only process current/one line.
            (when (<= (point) end-function-point)
              ;; NOTE(jenchieh): Store all the keyword name.
              (when (or (jcs-is-current-point-face "font-lock-keyword-face")
                        (jcs-is-current-point-face "font-lock-preprocessor-face"))
                (push (thing-at-point 'word) keyword-strings))

              ;; NOTE(jenchieh): Check if meet the function name.
              (when (or (jcs-is-current-point-face "font-lock-function-name-face")
                        (jcs-is-current-point-face "web-mode-function-name-face"))
                (setq function-name-string (thing-at-point 'word))
                (setq meet-function-name t))

              ;; NOTE(jenchieh): Store all the type name. (include return type name)
              (when (jcs-is-current-point-face "font-lock-type-face")
                ;; Just store it.
                (setq datatype-name (thing-at-point 'word))

                (if (or (not meet-function-name)
                        (< close-bracket-pt (point)))
                    (progn
                      (setq return-type-string (thing-at-point 'word))
                      (setq there-is-return t))
                  (progn
                    ;; NOTE(jenchieh): Since Lisp's default list data structure
                    ;; dose not support duplicate item in the list. Update the
                    ;; list by setting it to the brand new temporary list, which
                    ;; make muliple item list doable.
                    (let ((type-string (thing-at-point 'word))
                          (temp-list '()))
                      (push type-string temp-list)
                      (setq param-type-strings (append param-type-strings temp-list))))))

              ;; NOTE(jenchieh): Store all the variables name.
              (when (or (jcs-is-current-point-face "font-lock-variable-name-face")
                        (jcs-is-current-point-face 'js2-function-param)
                        (jcs-is-current-point-face "web-mode-variable-name-face")
                        (jcs-is-current-point-face "jcs-preproc-variable-name-face"))
                (push (thing-at-point 'word) param-variable-strings))
              )))))

    ;; Insert document comment string.
    (jcs-insert-doc-comment-string meet-function-name
                                   keyword-strings
                                   datatype-name
                                   function-name-string
                                   there-is-return
                                   return-type-string
                                   param-type-strings
                                   param-variable-strings
                                   search-string)))

(defun jcs-insert-doc-comment-string (meet-function-name
                                      keyword-strings
                                      datatype-name
                                      function-name-string
                                      there-is-return
                                      return-type-string
                                      param-type-strings
                                      param-variable-strings
                                      search-string)
  "Insert document comment style.

MEET-FUNCTION-NAME     : Meet the function name?
KEYWORD-STRINGS        : Keyword strings list.
DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
FUNCTION-NAME-STRING   : Function name.
THERE-IS-RETURN        : There is return in this function?
RETURN-TYPE-STRING     : String of the return type.
PARAM-TYPE-STRINGS     : Param type strings list.
PARAM-VARIABLE-STRINGS : Param name strings list.
PAREN-STRING           : Param raw string."

  (save-excursion
    (let ((mode-doc-string-func-name nil))
      ;; NOTE(jenchieh): `push' will push the element
      ;; at the front queue. `setq' and `append' will push
      ;; element from the back, so we need to reverse it
      ;; in order to match the order.
      (setq param-variable-strings (reverse param-variable-strings))

      (cond ((or (jcs-is-current-major-mode-p "actionscript-mode"))
             (setq mode-doc-string-func-name (if meet-function-name
                                                 'jcs-as-mode-doc-string-func
                                               'jcs-as-mode-doc-string-others)))
            ((or (jcs-is-current-major-mode-p "c-mode")
                 (jcs-is-current-major-mode-p "c++-mode"))
             (setq mode-doc-string-func-name (if meet-function-name
                                                 'jcs-cc-mode-doc-string-func
                                               'jcs-cc-mode-doc-string-others)))
            ((or (jcs-is-current-major-mode-p "csharp-mode"))
             (setq mode-doc-string-func-name (if meet-function-name
                                                 'jcs-csharp-mode-doc-string-func
                                               'jcs-csharp-mode-doc-string-others)))
            ((or (jcs-is-current-major-mode-p "java-mode")
                 (jcs-is-current-major-mode-p "jdee-mode"))
             (setq mode-doc-string-func-name (if meet-function-name
                                                 'jcs-java-mode-doc-string-func
                                               'jcs-java-mode-doc-string-others)))
            ((or (jcs-is-current-major-mode-p "js2-mode"))
             (setq mode-doc-string-func-name (if meet-function-name
                                                 'jcs-js-mode-doc-string-func
                                               'jcs-js-mode-doc-string-others)))
            ((or (jcs-is-current-major-mode-p "lua-mode"))
             (setq mode-doc-string-func-name (if meet-function-name
                                                 'jcs-lua-mode-doc-string-func
                                               'jcs-lua-mode-doc-string-others)))
            ((or (jcs-is-current-major-mode-p "python-mode"))
             (setq mode-doc-string-func-name (if meet-function-name
                                                 'jcs-py-mode-doc-string-func
                                               'jcs-py-mode-doc-string-others)))
            ((or (jcs-is-current-major-mode-p "php-mode")
                 (jcs-is-current-major-mode-p "web-mode"))
             (setq mode-doc-string-func-name (if meet-function-name
                                                 'jcs-php-mode-doc-string-func
                                               'jcs-php-mode-doc-string-others)))
            ((or (jcs-is-current-major-mode-p "typescript-mode"))
             (setq mode-doc-string-func-name (if meet-function-name
                                                 'jcs-ts-mode-doc-string-func
                                               'jcs-ts-mode-doc-string-others)))
            )

      ;; NOTE(jenchieh): Ensure the `mode-doc-string-func-name'
      ;; is assign to something valid to execute.
      (when mode-doc-string-func-name
        (if meet-function-name
            (funcall mode-doc-string-func-name
                     keyword-strings
                     datatype-name
                     function-name-string
                     there-is-return
                     return-type-string
                     param-type-strings
                     param-variable-strings
                     search-string)
          (funcall mode-doc-string-func-name))))))



(defun jcs-as-mode-doc-string-others ()
  "Insert `actionscript-mode' other doc string."
  (cond ((jcs-is-contain-list-string keyword-strings "class")
         (progn
           ;; TODO(jenchieh): implement into ActionScript mode.
           )))
  )

(defun jcs-as-mode-doc-string-func (keyword-strings
                                    datatype-name
                                    function-name-string
                                    there-is-return
                                    return-type-string
                                    param-type-strings
                                    param-variable-strings
                                    search-string)
  "Insert `actionscript-mode' function doc string.

KEYWORD-STRINGS        : Keyword strings list.
DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
FUNCTION-NAME-STRING   : Function name.
THERE-IS-RETURN        : There is return in this function?
RETURN-TYPE-STRING     : String of the return type.
PARAM-TYPE-STRINGS     : Param type strings list.
PARAM-VARIABLE-STRINGS : Param name strings list.
SEARCH-STRING          : Search raw string."

  (let ((param-var-len (length param-variable-strings))
        (param-index 0))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ;; start from newline.
      (insert "* @")
      (insert jcs-as-param-string)
      (when jcs-as-doc-show-typename
        (jcs-insert-jsdoc-type (nth param-index param-type-strings)
                               jcs-as-open-type-char
                               jcs-as-close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs-as-doc-after-value-type-char)
      (insert jcs-param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-string "void")
        (insert "\n")
        (insert "* @")
        (insert jcs-as-return-string)
        (when jcs-as-doc-show-typename
          (jcs-insert-jsdoc-type return-type-string
                                 jcs-as-open-type-char
                                 jcs-as-close-type-char))
        (backward-delete-char 1)
        (if jcs-as-doc-show-typename
            (insert jcs-as-doc-after-value-type-char)
          (insert " "))
        (insert jcs-return-desc-string)
        (indent-for-tab-command)))))


(defun jcs-cc-mode-doc-string-others ()
  "Insert `c-mode' or `c++-mode' other doc string."
  (cond ((jcs-is-contain-list-string-regexp keyword-strings "class")
         (progn
           ;; go back to comment line.
           (jcs-previous-line)
           (jcs-previous-line)
           (end-of-line)

           ;; Process class tag.
           (insert "@class ")
           (insert datatype-name)
           (indent-for-tab-command)

           ;; Process brief tag.
           (insert "\n")
           (insert "* @brief ")
           (insert jcs-class-desc-string)
           (indent-for-tab-command)))
        ((jcs-is-contain-list-string keyword-strings "struct")
         (progn
           ;; go back to comment line.
           (jcs-previous-line)
           (jcs-previous-line)
           (end-of-line)

           ;; Process class tag.
           (insert "@struct ")
           (insert datatype-name)
           (indent-for-tab-command)

           ;; Process brief tag.
           (insert "\n")
           (insert "* @brief ")
           (insert jcs-struct-desc-string)
           (indent-for-tab-command)))
        ((or (jcs-is-contain-list-string keyword-strings "define")
             (jcs-is-contain-list-string keyword-strings "#define"))
         (progn
           ;; go back to comment line.
           (jcs-previous-line)
           (jcs-previous-line)
           (end-of-line)

           ;; Process define tag.
           (insert "@def ")
           (insert (nth 0 param-variable-strings))
           (indent-for-tab-command)

           ;; Process brief tag.
           (insert "\n")
           (insert "* @brief ")
           (insert jcs-define-desc-string)
           (indent-for-tab-command)))
        ((jcs-is-contain-list-string keyword-strings "enum")
         (progn
           ;; go back to comment line.
           (jcs-previous-line)
           (jcs-previous-line)
           (end-of-line)

           ;; Process enumerator tag.
           (insert "@enum ")
           (insert datatype-name)
           (indent-for-tab-command)

           ;; Process brief tag.
           (insert "\n")
           (insert "* @brief ")
           (insert jcs-enum-desc-string)
           (indent-for-tab-command)))))

(defun jcs-cc-mode-doc-string-func (keyword-strings
                                    datatype-name
                                    function-name-string
                                    there-is-return
                                    return-type-string
                                    param-type-strings
                                    param-variable-strings
                                    search-string)
  "Insert `c-mode' or `c++-mode' function doc string.

KEYWORD-STRINGS        : Keyword strings list.
DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
FUNCTION-NAME-STRING   : Function name.
THERE-IS-RETURN        : There is return in this function?
RETURN-TYPE-STRING     : String of the return type.
PARAM-TYPE-STRINGS     : Param type strings list.
PARAM-VARIABLE-STRINGS : Param name strings list.
SEARCH-STRING          : Search raw string."

  ;; Get all the parameters.
  (let ((param-string "")
        (param-lst '())
        (param-type-str-lst '())
        (param-var-str-lst '()))
    (setq param-string (nth 1 (split-string search-string "(")))
    (setq param-string (nth 0 (split-string param-string ")")))

    (setq param-lst (split-string param-string ","))

    (let ((param-split-str-lst '())
          (param-split-str-lst-len -1)
          (param-var-str "")
          (param-type-str ""))
      (dolist (param-sec-string param-lst)
        (setq param-sec-string (nth 0 (split-string param-sec-string "=")))
        (setq param-split-str-lst (jcs-chop param-sec-string " "))
        (setq param-split-str-lst-len (length param-split-str-lst))
        ;; Variable name should always be that last element in the list.
        (setq param-var-str (string-trim (nth (1- param-split-str-lst-len) param-split-str-lst)))
        ;; Data type name should always be the second last element in the list.
        (setq param-type-str (string-trim (nth (- param-split-str-lst-len 2) param-split-str-lst)))

        (push param-var-str param-var-str-lst)
        (push param-type-str param-type-str-lst)))

    (setq param-type-strings (reverse param-type-str-lst))
    (setq param-variable-strings (reverse param-var-str-lst)))

  ;; Get the return data type.
  (setq return-type-string (nth 0 (split-string search-string " ")))

  (let ((param-var-len (length param-variable-strings))
        (param-index 0))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process Function name.
    (insert "@func ")
    (insert function-name-string)
    (indent-for-tab-command)

    ;; Process Breif description.
    (insert "\n")
    (insert "* @brief Function description here..")
    (indent-for-tab-command)

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ;; start from newline.
      (insert "* @")
      (insert jcs-cc-param-string)
      (when jcs-cc-doc-show-typename
        (jcs-insert-jsdoc-type (nth param-index param-type-strings)
                               jcs-cc-open-type-char
                               jcs-cc-close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs-cc-doc-after-value-type-char)
      (insert jcs-param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (if there-is-return
        (progn
          (if (not (string= return-type-string "void"))
              (progn
                (insert "\n")
                (insert "* @")
                (insert jcs-cc-return-string)
                (if jcs-cc-doc-show-typename
                    (jcs-insert-jsdoc-type return-type-string
                                           jcs-cc-open-type-char
                                           jcs-cc-close-type-char))
                (backward-delete-char 1)
                (if jcs-cc-doc-show-typename
                    (insert jcs-cc-doc-after-value-type-char)
                  (insert " "))
                (insert jcs-return-desc-string)
                (indent-for-tab-command)))))))


(defun jcs-csharp-mode-doc-string-others ()
  "Insert `csharp-mode' other doc string."
  (cond ((jcs-is-contain-list-string keyword-strings "class")
         (progn
           ;; STUDY(jenchieh): Don't think that C#
           ;; doc need one..
           ))
        ((jcs-is-contain-list-string keyword-strings "struct")
         (progn
           ;; STUDY(jenchieh): Don't think that C#
           ;; doc need one..
           ))
        ((or (jcs-is-contain-list-string keyword-strings "define")
             (jcs-is-contain-list-string keyword-strings "#define"))
         (progn
           ;; STUDY(jenchieh): Don't think that C#
           ;; doc need one..
           ))))

(defun jcs-csharp-mode-doc-string-func (keyword-strings
                                        datatype-name
                                        function-name-string
                                        there-is-return
                                        return-type-string
                                        param-type-strings
                                        param-variable-strings
                                        search-string)
  "Insert `csharp-mode' function doc string.

KEYWORD-STRINGS        : Keyword strings list.
DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
FUNCTION-NAME-STRING   : Function name.
THERE-IS-RETURN        : There is return in this function?
RETURN-TYPE-STRING     : String of the return type.
PARAM-TYPE-STRINGS     : Param type strings list.
PARAM-VARIABLE-STRINGS : Param name strings list.
SEARCH-STRING          : Search raw string."

  (let ((param-var-len (length param-variable-strings))
        (param-index 0))
    ;; go back to comment line.
    (jcs-previous-line)
    (end-of-line)

    ;; First process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ;; start from newline.
      (insert "/// <param name=\"")
      (insert (nth param-index param-variable-strings))
      (insert "\"></param>")

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-string "void")
        (insert "\n")
        (insert "/// <returns></returns>")
        (indent-for-tab-command)))))


(defun jcs-java-mode-doc-string-others ()
  "Insert `java-mode' other doc string."
  (cond ((jcs-is-contain-list-string keyword-strings "class")
         (progn
           ;; STUDY(jenchieh): Don't think that java
           ;; doc need one..
           ))
        ((jcs-is-contain-list-string keyword-strings "interface")
         (progn
           ;; STUDY(jenchieh): Don't think that java
           ;; doc need one..
           ))))

(defun jcs-java-mode-doc-string-func (keyword-strings
                                      datatype-name
                                      function-name-string
                                      there-is-return
                                      return-type-string
                                      param-type-strings
                                      param-variable-strings
                                      search-string)
  "Insert `java-mode' function doc string.

KEYWORD-STRINGS        : Keyword strings list.
DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
FUNCTION-NAME-STRING   : Function name.
THERE-IS-RETURN        : There is return in this function?
RETURN-TYPE-STRING     : String of the return type.
PARAM-TYPE-STRINGS     : Param type strings list.
PARAM-VARIABLE-STRINGS : Param name strings list.
SEARCH-STRING          : Search raw string."

  (let ((param-var-len (length param-variable-strings))
        (param-index 0))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ;; start from newline.
      (insert "* @")
      (insert jcs-java-param-string)
      (when jcs-java-doc-show-typename
        (jcs-insert-jsdoc-type (nth param-index param-type-strings)
                               jcs-java-open-type-char
                               jcs-java-close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs-java-doc-after-value-type-char)
      (insert jcs-param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-string "void")
        (insert "\n")
        (insert "* @")
        (insert jcs-java-return-string)
        (when jcs-java-doc-show-typename
          (jcs-insert-jsdoc-type return-type-string
                                 jcs-java-open-type-char
                                 jcs-java-close-type-char))
        (backward-delete-char 1)
        (if jcs-java-doc-show-typename
            (insert jcs-java-doc-after-value-type-char)
          (insert " "))
        (insert jcs-return-desc-string)
        (indent-for-tab-command)))))


(defun jcs-js-mode-doc-string-others ()
  "Insert `js2-mode' other doc string."
  (cond ((jcs-is-contain-list-string keyword-strings "class")
         (progn
           ;; STUDY(jenchieh): Don't know if javascript
           ;; need one..
           ))))

(defun jcs-js-mode-doc-string-func (keyword-strings
                                    datatype-name
                                    function-name-string
                                    there-is-return
                                    return-type-string
                                    param-type-strings
                                    param-variable-strings
                                    search-string)
  "Insert `js2-mode' function doc string.

KEYWORD-STRINGS        : Keyword strings list.
DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
FUNCTION-NAME-STRING   : Function name.
THERE-IS-RETURN        : There is return in this function?
RETURN-TYPE-STRING     : String of the return type.
PARAM-TYPE-STRINGS     : Param type strings list.
PARAM-VARIABLE-STRINGS : Param name strings list.
SEARCH-STRING          : Search raw string."

  (let ((param-var-len (length param-variable-strings))
        (param-index 0))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ;; start from newline.
      (insert "* @")
      (insert jcs-js-param-string)
      (when jcs-js-doc-show-typename
        (jcs-insert-jsdoc-type jcs-default-typename-string
                               jcs-js-open-type-char
                               jcs-js-close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs-js-doc-after-value-type-char)
      (insert jcs-param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-string "void")
        (insert "\n")
        (insert "* @")
        (insert jcs-js-return-string)
        (when jcs-js-doc-show-typename
          (jcs-insert-jsdoc-type return-type-string
                                 jcs-js-open-type-char
                                 jcs-js-close-type-char))
        (backward-delete-char 1)
        (if jcs-js-doc-show-typename
            (insert jcs-js-doc-after-value-type-char)
          (insert " "))
        (insert jcs-return-desc-string)
        (indent-for-tab-command)))))


(defun jcs-lua-mode-doc-string-others ()
  "Insert `lua-mode' other doc string."
  ;; NOTE(jenchieh): I don't think Lua have any keywords...
  )

(defun jcs-lua-mode-doc-string-func (keyword-strings
                                     datatype-name
                                     function-name-string
                                     there-is-return
                                     return-type-string
                                     param-type-strings
                                     param-variable-strings
                                     search-string)
  "Insert `lua-mode' function doc string.

KEYWORD-STRINGS        : Keyword strings list.
DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
FUNCTION-NAME-STRING   : Function name.
THERE-IS-RETURN        : There is return in this function?
RETURN-TYPE-STRING     : String of the return type.
PARAM-TYPE-STRINGS     : Param type strings list.
PARAM-VARIABLE-STRINGS : Param name strings list.
SEARCH-STRING          : Search raw string."

  (let ((param-var-len (length param-variable-strings))
        (param-index 0))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ;; start from newline.
      (insert "-- @")
      (insert jcs-lua-param-string)
      (when jcs-lua-doc-show-typename
        (jcs-insert-jsdoc-type jcs-default-typename-string
                               jcs-lua-open-type-char
                               jcs-lua-close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs-lua-doc-after-value-type-char)
      (insert jcs-param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-string "void")
        (insert "\n")
        (insert "-- @")
        (insert jcs-lua-return-string)
        (when jcs-lua-doc-show-typename
          (jcs-insert-jsdoc-type return-type-string
                                 jcs-lua-open-type-char
                                 jcs-lua-close-type-char))
        (backward-delete-char 1)
        (if jcs-lua-doc-show-typename
            (insert jcs-lua-doc-after-value-type-char)
          (insert " "))
        (insert jcs-return-desc-string)
        (indent-for-tab-command)))))


(defun jcs-py-mode-doc-string-others ()
  "Insert `python-mode' other doc string."
  (cond ((jcs-is-contain-list-string keyword-strings "class")
         (progn
           ;; TODO(jenchieh): implement into python mode.
           ))))

(defun jcs-py-mode-doc-string-func (keyword-strings
                                    datatype-name
                                    function-name-string
                                    there-is-return
                                    return-type-string
                                    param-type-strings
                                    param-variable-strings
                                    search-string)
  "Insert `python-mode' function doc string.

KEYWORD-STRINGS        : Keyword strings list.
DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
FUNCTION-NAME-STRING   : Function name.
THERE-IS-RETURN        : There is return in this function?
RETURN-TYPE-STRING     : String of the return type.
PARAM-TYPE-STRINGS     : Param type strings list.
PARAM-VARIABLE-STRINGS : Param name strings list.
SEARCH-STRING          : Search raw string."

  (let ((param-var-len (length param-variable-strings))
        (param-index 0))
    ;; go back to comment line.
    (jcs-move-to-forward-a-char-recursive "\"")
    (jcs-move-to-forward-a-char-recursive "\"")
    (jcs-move-to-forward-a-char-recursive "\"")

    (when (= jcs-py-doc-string-version 1)
      ;; OPTION(jenchieh): docstring option..
      (jcs-next-line))
    (end-of-line)

    ;; Line breack between description and tags.
    (if (>= param-index 0)
        (insert "\n"))

    (while (< param-index param-var-len)
      (when (not (string= "self" (nth param-index param-variable-strings)))
        (insert "\n")  ;; start from newline.
        (insert "@")
        (insert jcs-py-param-string)
        (when jcs-py-doc-show-typename
          (jcs-insert-jsdoc-type jcs-default-typename-string
                                 jcs-py-open-type-char
                                 jcs-py-close-type-char))
        (insert (nth param-index param-variable-strings))
        (insert jcs-py-doc-after-value-type-char)
        (insert jcs-param-desc-string)

        ;; indent once.
        (indent-for-tab-command))

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-string "void")
        (insert "\n")
        (insert "@")
        (insert jcs-py-return-string)
        (when jcs-py-doc-show-typename
          (jcs-insert-jsdoc-type return-type-string
                                 jcs-py-open-type-char
                                 jcs-py-close-type-char))
        (backward-delete-char 1)
        (if jcs-py-doc-show-typename
            (insert jcs-py-doc-after-value-type-char)
          (insert " "))
        (insert jcs-return-desc-string)
        (indent-for-tab-command)))))


(defun jcs-php-mode-doc-string-others ()
  "Insert `php-mode' other doc string."
  (cond ((jcs-is-contain-list-string keyword-strings "class")
         (progn
           ;; TODO(jenchieh): implement into PHP mode.
           ))))

(defun jcs-php-mode-doc-string-func (keyword-strings
                                     datatype-name
                                     function-name-string
                                     there-is-return
                                     return-type-string
                                     param-type-strings
                                     param-variable-strings
                                     search-string)
  "Insert `php-mode' function doc string.

KEYWORD-STRINGS        : Keyword strings list.
DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
FUNCTION-NAME-STRING   : Function name.
THERE-IS-RETURN        : There is return in this function?
RETURN-TYPE-STRING     : String of the return type.
PARAM-TYPE-STRINGS     : Param type strings list.
PARAM-VARIABLE-STRINGS : Param name strings list.
SEARCH-STRING          : Search raw string."

  (let ((param-var-len (length param-variable-strings))
        (param-index 0))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ;; start from newline.
      (insert "* @")
      (insert jcs-php-param-string)
      (when jcs-php-doc-show-typename
        (jcs-insert-jsdoc-type jcs-default-typename-string
                               jcs-php-open-type-char
                               jcs-php-close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs-php-doc-after-value-type-char)
      (insert jcs-param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-string "void")
        (insert "\n")
        (insert "* @")
        (insert jcs-php-return-string)
        (when jcs-php-doc-show-typename
          (jcs-insert-jsdoc-type return-type-string
                                 jcs-php-open-type-char
                                 jcs-php-close-type-char))
        (backward-delete-char 1)
        (if jcs-php-doc-show-typename
            (insert jcs-php-doc-after-value-type-char)
          (insert " "))
        (insert jcs-return-desc-string)
        (indent-for-tab-command)))))


(defun jcs-ts-mode-doc-string-others ()
  "Insert `typescript-mode' other doc string."
  (cond ((jcs-is-contain-list-string keyword-strings "class")
         (progn
           ;; TODO(jenchieh): implement into TypeScript mode.
           ))))

(defun jcs-ts-mode-doc-string-func (keyword-strings
                                    datatype-name
                                    function-name-string
                                    there-is-return
                                    return-type-string
                                    param-type-strings
                                    param-variable-strings
                                    search-string)
  "Insert `typescript-mode' function doc string.

KEYWORD-STRINGS        : Keyword strings list.
DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
FUNCTION-NAME-STRING   : Function name.
THERE-IS-RETURN        : There is return in this function?
RETURN-TYPE-STRING     : String of the return type.
PARAM-TYPE-STRINGS     : Param type strings list.
PARAM-VARIABLE-STRINGS : Param name strings list.
SEARCH-STRING          : Search raw string."

  ;; Get all the parameters.
  (let ((param-string "")
        (param-lst '())
        (param-type-str-lst '())
        (param-var-str-lst '()))
    (setq param-string (nth 1 (split-string search-string "(")))
    (setq param-string (nth 0 (split-string param-string ")")))

    (setq param-lst (split-string param-string ","))

    (let ((param-split-str-lst '())
          (param-var-str "")
          (param-type-str ""))
      (dolist (param-sec-string param-lst)
        ;; First remove the possible default value.
        (setq param-sec-string (nth 0 (split-string param-sec-string "=")))
        (setq param-split-str-lst (split-string param-sec-string ":"))
        (setq param-var-str (string-trim (nth 0 param-split-str-lst)))
        (if (= (length param-split-str-lst) 1)
            ;; Set default type name string here.
            (setq param-type-str jcs-default-typename-string)
          (setq param-type-str (string-trim (nth 1 param-split-str-lst))))

        (push param-var-str param-var-str-lst)
        (push param-type-str param-type-str-lst)))

    (setq param-type-strings (reverse param-type-str-lst))
    (setq param-variable-strings (reverse param-var-str-lst)))

  ;; Get all return data types.
  (setq return-type-string (nth 1 (split-string search-string ")")))
  (setq return-type-string (string-trim (nth 1 (split-string return-type-string ":"))))


  (let* ((param-var-len (length param-variable-strings))
         (param-type-len (length param-type-strings))
         (keyword-len (length keyword-strings))
         (param-index 0)
         (func-keyword (nth (1- keyword-len) keyword-strings)))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    (insert "@desc ")
    (indent-for-tab-command)

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n")  ;; start from newline.
      (insert "* @")
      (insert jcs-ts-param-string)
      (when jcs-ts-doc-show-typename
        (jcs-insert-jsdoc-type (nth param-index param-type-strings)
                               jcs-ts-open-type-char
                               jcs-ts-close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs-ts-doc-after-value-type-char)
      (insert jcs-param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (insert "\n")
      (insert "* @")
      (insert jcs-ts-return-string)
      (when jcs-ts-doc-show-typename
        (jcs-insert-jsdoc-type return-type-string
                               jcs-ts-open-type-char
                               jcs-ts-close-type-char))
      (backward-delete-char 1)
      (if jcs-ts-doc-show-typename
          (insert jcs-js-doc-after-value-type-char)
        (insert " "))
      (insert jcs-return-desc-string)
      (indent-for-tab-command))))


;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defvar jcs-oop-missing-font-lock-variable-name-modes-strict '(c-mode)
  "Modes to fixed missing font lock variable name face in strict programming language.")

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("([ \t]*[a-zA-Z_$0-9[&* \t]* \\([a-zA-Z_$0-9[&* \t]*\\)[ \t]*," 1 'font-lock-variable-name-face t)
           ("([ \t]*[a-zA-Z_$0-9[&* \t]* [a-zA-Z_$0-9[&* \t]* \\([a-zA-Z_$0-9[&* \t]*\\)[ \t]*," 1 'font-lock-variable-name-face t)
           ;; Require for two word variables.
           ;; For instance, `const'.
           (",[ \t]*[a-zA-Z_$0-9[&* \t]* \\([a-zA-Z_$0-9[&* \t]*\\)[ \t]*," 1 'font-lock-variable-name-face t)
           (",[ \t]*[a-zA-Z_$0-9[&* \t]* [a-zA-Z_$0-9[&* \t]* \\([a-zA-Z_$0-9[&* \t]*\\)[ \t]*," 1 'font-lock-variable-name-face t)
           ;; For line break parameter declaration.
           ("^[ \t] [a-zA-Z_$0-9[&* \t]* [a-zA-Z_$0-9[&* \t]* \\([a-zA-Z_$0-9[&* \t]*\\)[ \t]*[,)]" 1 'font-lock-variable-name-face t)
           )'end))
      jcs-oop-missing-font-lock-variable-name-modes-strict)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defvar jcs-oop-missing-font-lock-type-face-modes '(c++-mode)
  "Font lock for namespace in C++.")

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("[a-zA-Z0-9_]*::\\([a-zA-Z0-9_]*\\)[ \t]" 1 'font-lock-type-face t)
           )'end))
      jcs-oop-missing-font-lock-type-face-modes)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defvar jcs-oop-missing-font-lock-variable-name-modes '(lua-mode
                                                        php-mode
                                                        python-mode)
  "Modes to fixed missing font lock variable name face.")

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("(,*\\([a-zA-Z_$0-9 \t]*\\)[,)]" 1 'font-lock-variable-name-face t)
           (",\\([a-zA-Z_$0-9, \t]*\\)," 1 'font-lock-variable-name-face t)
           ("\\([a-zA-Z_$0-9 \t]*\\)[)]" 1 'font-lock-variable-name-face t)
           ;; For line break parameter declaration.
           ("^[ \t]* \\([a-zA-Z_$0-9,]*\\)[ \t]*[,)]" 1 'font-lock-variable-name-face t)
           )'end))
      jcs-oop-missing-font-lock-variable-name-modes)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defvar jcs-oop-missing-font-lock-variable-name-modes-colon '(actionscript-mode
                                                              typescript-mode)
  "Modes to fixed missing font lock variable name face in programming language that uses `colon'.")

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("(,*\\([a-zA-Z_$0-9 \t]*\\)[,)]" 1 'font-lock-variable-name-face t)
           (",\\([a-zA-Z_$0-9, \t]*\\)," 1 'font-lock-variable-name-face t)
           (",\\([a-zA-Z_$0-9 \t]*\\)[)]" 1 'font-lock-variable-name-face t)
           ;; For line break parameter declaration.
           ("^[ \t]* \\([a-zA-Z_$0-9,]*\\)[ \t]*[,)]" 1 'font-lock-variable-name-face t)
           ;; With colon.
           ("(,*\\([a-zA-Z_$0-9 \t]*\\)[:,)]" 1 'font-lock-variable-name-face t)
           (",\\([a-zA-Z_$0-9, \t]*\\):" 1 'font-lock-variable-name-face t)
           )'end))
      jcs-oop-missing-font-lock-variable-name-modes-colon)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defvar jcs-oop-missing-font-lock-func-name-modes '(actionscript-mode
                                                    typescript-mode)
  "Modes to fixed missing function name face.")

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("function[ \t]*\\([a-zA-Z_$0-9]*\\)[ \t]*(" 1 'font-lock-function-name-face t)
           ("public[ \t]*\\([a-zA-Z_$0-9]*\\)[ \t]*(" 1 'font-lock-function-name-face t)
           ("private[ \t]*\\([a-zA-Z_$0-9]*\\)[ \t]*(" 1 'font-lock-function-name-face t)
           ("protected[ \t]*\\([a-zA-Z_$0-9]*\\)[ \t]*(" 1 'font-lock-function-name-face t)
           )'end))
      jcs-oop-missing-font-lock-func-name-modes)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defvar jcs-oop-missing-font-lock-type-face-modes-colon '(actionscript-mode
                                                          typescript-mode)
  "Modes to fixed missing type face in programming language using `colon'.")

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("[|:][ \t\n]*\\([a-zA-Z0-9_-]*\\)[.][a-zA-Z0-9_-]*[ \t\n]*[|=),{]" 1 'font-lock-type-face t)
           ("[|:][ \t\n]*[a-zA-Z0-9_-]*[.]\\([a-zA-Z0-9_-]*\\)[ \t\n]*[|=),{]" 1 'font-lock-type-face t)
           ("[|:][ \t\n]*\\([a-zA-Z0-9_-]*\\)[ \t\n]*[|{]" 1 'font-lock-type-face t)
           ("[|:][ \t\n]*\\([a-zA-Z0-9_-]*\\)[ \t\n]*[=),]" 1 'font-lock-type-face t)
           )'end))
      jcs-oop-missing-font-lock-type-face-modes-colon)


(provide 'jcs-oop-func)
;;; jcs-oop-func.el ends here
