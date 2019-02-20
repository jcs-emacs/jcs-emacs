;; ========================================================================
;; $File: jcs-oop-func.el $
;; $Date: 2017-11-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions for Object Oriented Programming languages.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defvar jcs-oop-highlight-modes '(actionscript-mode
                                  cc-mode
                                  c-mode
                                  c++-mode
                                  csharp-mode
                                  java-mode
                                  ;;jdee-mode  ;; Java has their own doc highlighting.
                                  jayces-mode
                                  js2-mode
                                  lua-mode
                                  nasm-mode
                                  php-mode
                                  python-mode
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
             ("@[a-zA-Z0-9_].*[\]\|}]\\([a-zA-Z0-9_$ \t.]*\\)[:-]" 1 'jcs-oop-value-face t)
             ;;
             ;; NOTE(jenchieh):
             ;; Doc String Style:
             ;;
             ;; @param `ValueTag' : value tag description..
             ;;
             ("@[a-zA-Z0-9_]*\\([a-zA-Z0-9_ \t.]*\\)[{:-]" 1 'jcs-oop-value-face t)
             )'end))
        jcs-oop-highlight-modes))


;;; Doc string
(defvar jcs-py-doc-string-version 0
  "Document string version.

0 : Description after \"\"\" opening docstring..
1 : Line breack description \"\"\" opening docstring.")

;;; Doc string character after value type font.
(defvar jcs-java-doc-after-value-type-char ""
  "Character after value type been inserted in Java Mode.")
(defvar jcs-cc-doc-after-value-type-char ""
  "Character after value type been inserted in C/C++ Mode.")
(defvar jcs-js-doc-after-value-type-char ""
  "Character after value type been inserted in JavaScript Mode.")
(defvar jcs-lua-doc-after-value-type-char ""
  "Character after value type been inserted in Lua Mode.")
(defvar jcs-py-doc-after-value-type-char ""
  "Character after value type been inserted in Python Mode.")
(defvar jcs-php-doc-after-value-type-char ""
  "Character after value type been inserted in PHP Mode.")

;;; Show typename.
(defvar jcs-java-doc-show-typename t
  "Show the typename betweeen the open charachter and close charachter in Java mode.")
(defvar jcs-cc-doc-show-typename t
  "Show the typename betweeen the open charachter and close charachter in C/C++ mode.")
(defvar jcs-js-doc-show-typename t
  "Show the typename betweeen the open charachter and close charachter in JavaScript mode.")
(defvar jcs-lua-doc-show-typename t
  "Show the typename betweeen the open charachter and close charachter in Lua mode.")
(defvar jcs-py-doc-show-typename t
  "Show the typename betweeen the open charachter and close charachter in Python mode.")
(defvar jcs-php-doc-show-typename t
  "Show the typename betweeen the open charachter and close charachter in PHP mode.")

;;; Tag strings
(defvar jcs-java-param-string ""
  "Parameter string in Java mode.")
(defvar jcs-java-return-string ""
  "Returns string in Java mode.")

(defvar jcs-cc-param-string ""
  "Parameter string in C/C++ mode.")
(defvar jcs-cc-return-string ""
  "Returns string in C/C++ mode.")

(defvar jcs-js-param-string ""
  "Parameter string in JavaScript mode.")
(defvar jcs-js-return-string ""
  "Returns string in JavaScript mode.")

(defvar jcs-lua-param-string ""
  "Parameter string in Lua mode.")
(defvar jcs-lua-return-string ""
  "Returns string in Lua mode.")

(defvar jcs-py-param-string ""
  "Parameter string in Pyhon mode.")
(defvar jcs-py-return-string ""
  "Returns string in Python mode.")

(defvar jcs-php-param-string ""
  "Parameter string in PHP mode.")
(defvar jcs-php-return-string ""
  "Returns string in PHP mode.")

;;; Brackets
(defvar jcs-java-open-type-char ""
  "Character before the typename in Java mode.")
(defvar jcs-java-close-type-char ""
  "Character after the typename in Java mode.")

(defvar jcs-cc-open-type-char ""
  "Character before the typename in C/C++ mode.")
(defvar jcs-cc-close-type-char ""
  "Character after the typename in C/C++ mode.")

(defvar jcs-js-open-type-char ""
  "Character before the typename in JavaScript mode.")
(defvar jcs-js-close-type-char ""
  "Character after the typename in JavaScript mode.")

(defvar jcs-lua-open-type-char ""
  "Character before the typename in Lua mode.")
(defvar jcs-lua-close-type-char ""
  "Character after the typename in Lua mode.")

(defvar jcs-py-open-type-char ""
  "Character before the typename in Python mode.")
(defvar jcs-py-close-type-char ""
  "Character after the typename in Python mode.")

(defvar jcs-php-open-type-char ""
  "Character before the typename in PHP mode.")
(defvar jcs-php-close-type-char ""
  "Character after the typename in PHP mode.")


(defvar jcs-class-desc-string ""
  "Class description string.")
(defvar jcs-struct-desc-string ""
  "Struct description string.")
(defvar jcs-define-desc-string ""
  "Define description string.")
(defvar jcs-enum-desc-string ""
  "Enum description string.")
(defvar jcs-param-desc-string ""
  "Param description string.")
(defvar jcs-return-desc-string ""
  "Return description string.")

(defvar jcs-docstring-config-filepath "~/.emacs.jcs/docstring/docstring_config.properties"
  "Doc-string properties file.")

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

    ;; show type name
    (setq jcs-java-doc-show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "JAVA_DOC_SHOW_TYPENAME")))
    (setq jcs-cc-doc-show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "CC_DOC_SHOW_TYPENAME")))
    (setq jcs-js-doc-show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "JS_DOC_SHOW_TYPENAME")))
    (setq jcs-lua-doc-show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "LUA_DOC_SHOW_TYPENAME")))
    (setq jcs-py-doc-show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "PY_DOC_SHOW_TYPENAME")))
    (setq jcs-php-doc-show-typename (jcs-parse-bool (jcs-get-properties tmp-ini-list "PHP_DOC_SHOW_TYPENAME")))

    ;; After value type character.
    (setq jcs-java-doc-after-value-type-char (jcs-get-properties tmp-ini-list "JAVA_AFTER_VALUE_TYPE"))
    (setq jcs-cc-doc-after-value-type-char (jcs-get-properties tmp-ini-list "CC_AFTER_VALUE_TYPE"))
    (setq jcs-js-doc-after-value-type-char (jcs-get-properties tmp-ini-list "JS_AFTER_VALUE_TYPE"))
    (setq jcs-lua-doc-after-value-type-char (jcs-get-properties tmp-ini-list "LUA_AFTER_VALUE_TYPE"))
    (setq jcs-py-doc-after-value-type-char (jcs-get-properties tmp-ini-list "PY_AFTER_VALUE_TYPE"))
    (setq jcs-php-doc-after-value-type-char (jcs-get-properties tmp-ini-list "PHP_AFTER_VALUE_TYPE"))

    ;; param string
    (setq jcs-java-param-string (jcs-get-properties tmp-ini-list "JAVA_PARAM_STRING"))
    (setq jcs-cc-param-string (jcs-get-properties tmp-ini-list "CC_PARAM_STRING"))
    (setq jcs-js-param-string (jcs-get-properties tmp-ini-list "JS_PARAM_STRING"))
    (setq jcs-lua-param-string (jcs-get-properties tmp-ini-list "LUA_PARAM_STRING"))
    (setq jcs-py-param-string (jcs-get-properties tmp-ini-list "PY_PARAM_STRING"))
    (setq jcs-php-param-string (jcs-get-properties tmp-ini-list "PHP_PARAM_STRING"))

    ;; return string
    (setq jcs-java-return-string (jcs-get-properties tmp-ini-list "JAVA_RETURN_STRING"))
    (setq jcs-cc-return-string (jcs-get-properties tmp-ini-list "CC_RETURN_STRING"))
    (setq jcs-js-return-string (jcs-get-properties tmp-ini-list "JS_RETURN_STRING"))
    (setq jcs-lua-return-string (jcs-get-properties tmp-ini-list "LUA_RETURN_STRING"))
    (setq jcs-py-return-string (jcs-get-properties tmp-ini-list "PY_RETURN_STRING"))
    (setq jcs-php-return-string (jcs-get-properties tmp-ini-list "PHP_RETURN_STRING"))

    ;; open type character.
    (setq jcs-java-open-type-char (jcs-get-properties tmp-ini-list "JAVA_OPEN_TYPE_CHAR"))
    (setq jcs-cc-open-type-char (jcs-get-properties tmp-ini-list "CC_OPEN_TYPE_CHAR"))
    (setq jcs-js-open-type-char (jcs-get-properties tmp-ini-list "JS_OPEN_TYPE_CHAR"))
    (setq jcs-lua-open-type-char (jcs-get-properties tmp-ini-list "LUA_OPEN_TYPE_CHAR"))
    (setq jcs-py-open-type-char (jcs-get-properties tmp-ini-list "PY_OPEN_TYPE_CHAR"))
    (setq jcs-php-open-type-char (jcs-get-properties tmp-ini-list "PHP_OPEN_TYPE_CHAR"))

    ;; close type character.
    (setq jcs-java-close-type-char (jcs-get-properties tmp-ini-list "JAVA_CLOSE_TYPE_CHAR"))
    (setq jcs-cc-close-type-char (jcs-get-properties tmp-ini-list "CC_CLOSE_TYPE_CHAR"))
    (setq jcs-js-close-type-char (jcs-get-properties tmp-ini-list "JS_CLOSE_TYPE_CHAR"))
    (setq jcs-lua-close-type-char (jcs-get-properties tmp-ini-list "LUA_CLOSE_TYPE_CHAR"))
    (setq jcs-py-close-type-char (jcs-get-properties tmp-ini-list "PY_CLOSE_TYPE_CHAR"))
    (setq jcs-php-close-type-char (jcs-get-properties tmp-ini-list "PHP_CLOSE_TYPE_CHAR"))
    ))


(defun jcs-move-cursor-by-search-option (search-option)
  "Move to next targeting end function character.
SEARCH-OPTION :
0) search only current line.
1) search witch closing parenthesis.
2) search with opening culry parenthesis."

  (ignore-errors
    (cond ((eq search-option 0)
           (progn
             ;; Only the current line.
             (end-of-line)
             ))
          ((eq search-option 1)
           (progn
             ;; Closing Parenthesis
             ;; NOTE(jenchieh): No recursive/No prompt.
             (jcs-move-forward-close-paren t)
             ))
          ((eq search-option 2)
           (progn
             ;; Opening Curly Parenthesis
             ;; NOTE(jenchieh): No recursive/No prompt.
             (jcs-move-forward-open-curlyParen t)
             )))))

(defun jcs-insert-comment-style-by-current-line (search-option)
  "Read the current line and insert by reading the need from \
the input line.

SEARCH-OPTION :
0) search only current line.
1) search witch closing parenthesis.
2) search with opening culry parenthesis."
  (interactive)

  (let ((keyword-strings '())
        (datatype-name "")
        (meet-function-name nil)
        (function-name-string "")
        (param-type-strings '())  ;; param type string list.
        (param-variable-strings '())  ;; param name string list.
        (there-is-return nil)
        (return-type-string "")

        (was-flycheck-on nil)
        (was-flymake-on nil))
    (when (jcs-is-minor-mode-enabled-p flycheck-mode)
      (setq was-flycheck-on t))

    (when (jcs-is-minor-mode-enabled-p flymake-mode)
      (setq was-flymake-on t))

    (flycheck-mode 0)
    (flymake-mode 0)

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
                (jcs-move-cursor-by-search-option search-option)

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

          (while (< (point) end-function-point)
            (if (not (= word-index 0))
                (forward-word))
            (forward-word)
            (backward-char 1)
            (setq word-index (1+ word-index))

            ;; Make sure only process current/one line.
            (when (<= (point) end-function-point)
              ;; NOTE(jenchieh): Store all the keyword name.
              (when (or (jcs-is-current-point-face "font-lock-keyword-face")
                        (jcs-is-current-point-face "font-lock-preprocessor-face"))
                (add-to-list 'keyword-strings (thing-at-point 'word)))

              ;; NOTE(jenchieh): Check if meet the function name.
              (when (or (jcs-is-current-point-face "font-lock-function-name-face")
                        (jcs-is-current-point-face "web-mode-function-name-face"))
                (setq function-name-string (thing-at-point 'word))
                (setq meet-function-name t))

              ;; NOTE(jenchieh): Store all the type name. (include return type name)
              (when (jcs-is-current-point-face "font-lock-type-face")
                ;; Just store it.
                (setq datatype-name (thing-at-point 'word))

                (if (not (equal meet-function-name t))
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
                      (add-to-list 'temp-list type-string)
                      (setq param-type-strings (append param-type-strings temp-list))))))

              ;; NOTE(jenchieh): Store all the variables name.
              (when (or (jcs-is-current-point-face "font-lock-variable-name-face")
                        (jcs-is-current-point-face 'js2-function-param)
                        (jcs-is-current-point-face "web-mode-variable-name-face")
                        (jcs-is-current-point-face "jcs-preproc-variable-name-face"))
                (add-to-list 'param-variable-strings (thing-at-point 'word))))))))

    ;; Insert document comment string.
    (jcs-insert-doc-comment-string meet-function-name
                                   keyword-strings
                                   datatype-name
                                   function-name-string
                                   there-is-return
                                   return-type-string
                                   param-type-strings
                                   param-variable-strings)

    ;; Enable it back on if it was on.
    (when was-flycheck-on
      (flycheck-mode t))
    (when was-flymake-on
      (flymake-mode t))))

(defun jcs-insert-doc-comment-string (meet-function-name
                                      keyword-strings
                                      datatype-name
                                      function-name-string
                                      there-is-return
                                      return-type-string
                                      param-type-strings
                                      param-variable-strings)
  "Insert document comment style.

@param MEET-FUNCTION-NAME     : Meet the function name?
@param KEYWORD-STRINGS        : Keyword strings list.
@param DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
@param FUNCTION-NAME-STRING   : Function name.
@param THERE-IS-RETURN        : There is return in this function?
@param RETURN-TYPE-STRING     : String of the return type.
@param PARAM-TYPE-STRINGS     : Param type strings list.
@param PARAM-VARIABLE-STRINGS : Param name strings list."
  (interactive)

  (save-excursion

    ;; Only add doc when there is function in current
    ;; checking line.
    (if (equal meet-function-name t)
        (progn
          (let (;; This could either use `param-type-strings' or
                ;; `param-variable-strings' because they should have
                ;; the same length.
                (param-len (length param-variable-strings))
                (param-index (1- (length param-variable-strings))))

            ;; NOTE(jenchieh): `add-to-list' will push the element
            ;; at the front queue. `setq' and `append' will push
            ;; element from the back, so we need to reverse it
            ;; in order to match the order.
            (setq param-type-strings (reverse param-type-strings))

            (jcs-csharp-mode-doc-string meet-function-name
                                        keyword-strings
                                        datatype-name
                                        function-name-string
                                        there-is-return
                                        return-type-string
                                        param-type-strings
                                        param-variable-strings)

            (jcs-cc-mode-doc-string meet-function-name
                                    keyword-strings
                                    datatype-name
                                    function-name-string
                                    there-is-return
                                    return-type-string
                                    param-type-strings
                                    param-variable-strings)

            (jcs-java-mode-doc-string meet-function-name
                                      keyword-strings
                                      datatype-name
                                      function-name-string
                                      there-is-return
                                      return-type-string
                                      param-type-strings
                                      param-variable-strings)

            (jcs-js-mode-doc-string meet-function-name
                                    keyword-strings
                                    datatype-name
                                    function-name-string
                                    there-is-return
                                    return-type-string
                                    param-type-strings
                                    param-variable-strings)

            (jcs-lua-mode-doc-string meet-function-name
                                     keyword-strings
                                     datatype-name
                                     function-name-string
                                     there-is-return
                                     return-type-string
                                     param-type-strings
                                     param-variable-strings)

            (jcs-py-mode-doc-string meet-function-name
                                    keyword-strings
                                    datatype-name
                                    function-name-string
                                    there-is-return
                                    return-type-string
                                    param-type-strings
                                    param-variable-strings)

            (jcs-php-mode-doc-string meet-function-name
                                     keyword-strings
                                     datatype-name
                                     function-name-string
                                     there-is-return
                                     return-type-string
                                     param-type-strings
                                     param-variable-strings)))
      ;; NOTE(jenchieh): Design object comment document string.
      ;; For instance, macro define, struct, class, etc.
      (progn

        (when (jcs-is-current-major-mode-p "csharp-mode")
          (cond ((jcs-is-in-list-string keyword-strings "class")
                 (progn
                   ;; STUDY(jenchieh): Don't think that C#
                   ;; doc need one..
                   ))
                ((jcs-is-in-list-string keyword-strings "struct")
                 (progn
                   ;; STUDY(jenchieh): Don't think that C#
                   ;; doc need one..
                   ))
                ((or (jcs-is-in-list-string keyword-strings "define")
                     (jcs-is-in-list-string keyword-strings "#define"))
                 (progn
                   ;; STUDY(jenchieh): Don't think that C#
                   ;; doc need one..
                   ))))

        (when (or (jcs-is-current-major-mode-p "c++-mode")
                  (jcs-is-current-major-mode-p "c-mode"))
          (cond ((jcs-is-in-list-string keyword-strings "class")
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
                ((jcs-is-in-list-string keyword-strings "struct")
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
                ((or (jcs-is-in-list-string keyword-strings "define")
                     (jcs-is-in-list-string keyword-strings "#define"))
                 (progn
                   ;; go back to comment line.
                   (jcs-previous-line)
                   (jcs-previous-line)
                   (end-of-line)

                   ;; NOTE(jenchieh): `add-to-list' will push the element
                   ;; at the front queue. `setq' and `append' will push
                   ;; element from the back, so we need to reverse it
                   ;; in order to match the order.
                   ;;
                   ;; Reverse once.
                   (setq param-variable-strings (reverse param-variable-strings))

                   ;; Process define tag.
                   (insert "@def ")
                   (insert (nth 0 param-variable-strings))
                   (indent-for-tab-command)

                   ;; Process brief tag.
                   (insert "\n")
                   (insert "* @brief ")
                   (insert jcs-define-desc-string)
                   (indent-for-tab-command)))
                ((jcs-is-in-list-string keyword-strings "enum")
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

        (when (or (jcs-is-current-major-mode-p "java-mode")
                  (jcs-is-current-major-mode-p "jdee-mode"))
          (cond ((jcs-is-in-list-string keyword-strings "class")
                 (progn
                   ;; STUDY(jenchieh): Don't think that java
                   ;; doc need one..
                   ))
                ((jcs-is-in-list-string keyword-strings "interface")
                 (progn
                   ;; STUDY(jenchieh): Don't think that java
                   ;; doc need one..
                   ))))

        (when (or (jcs-is-current-major-mode-p "js2-mode"))
          (cond ((jcs-is-in-list-string keyword-strings "class")
                 (progn
                   ;; STUDY(jenchieh): Don't know if javascript
                   ;; need one..
                   ))))

        (when (or (jcs-is-current-major-mode-p "lua-mode"))
          ;; NOTE(jenchieh): I don't think Lua have any keywords...
          )

        (when (or (jcs-is-current-major-mode-p "python-mode"))
          (cond ((jcs-is-in-list-string keyword-strings "class")
                 (progn
                   ;; TODO(jenchieh): implement into python mode.
                   ))))

        (when (or (jcs-is-current-major-mode-p "php-mode")
                  (jcs-is-current-major-mode-p "web-mode"))
          (cond ((jcs-is-in-list-string keyword-strings "class")
                 (progn
                   ;; TODO(jenchieh): implement into PHP mode.
                   ))))
        ))))


(defun jcs-csharp-mode-doc-string (meet-function-name
                                   keyword-strings
                                   datatype-name
                                   function-name-string
                                   there-is-return
                                   return-type-string
                                   param-type-strings
                                   param-variable-strings)
  "Insert `csharp-mode' doc string.

@param MEET-FUNCTION-NAME     : Meet the function name?
@param KEYWORD-STRINGS        : Keyword strings list.
@param DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
@param FUNCTION-NAME-STRING   : Function name.
@param THERE-IS-RETURN        : There is return in this function?
@param RETURN-TYPE-STRING     : String of the return type.
@param PARAM-TYPE-STRINGS     : Param type strings list.
@param PARAM-VARIABLE-STRINGS : Param name strings list.
"
  (when (or (jcs-is-current-major-mode-p "csharp-mode"))
    ;; go back to comment line.
    (jcs-previous-line)
    (end-of-line)

    ;; First process param tag.
    (while (>= param-index 0)
      (insert "\n")  ;; start from newline.
      (insert "/// <param name=\"")
      (insert (nth param-index param-variable-strings))
      (insert "\"></param>")

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1- param-index)))

    ;; Lastly, process returns tag.
    (when (equal there-is-return t)
      (when (not(string= return-type-string "void"))
        (insert "\n")
        (insert "/// <returns></returns>")
        (indent-for-tab-command)))))


(defun jcs-cc-mode-doc-string (meet-function-name
                               keyword-strings
                               datatype-name
                               function-name-string
                               there-is-return
                               return-type-string
                               param-type-strings
                               param-variable-strings)
  "Insert `c-mode' or `c++-mode' doc string.

@param MEET-FUNCTION-NAME     : Meet the function name?
@param KEYWORD-STRINGS        : Keyword strings list.
@param DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
@param FUNCTION-NAME-STRING   : Function name.
@param THERE-IS-RETURN        : There is return in this function?
@param RETURN-TYPE-STRING     : String of the return type.
@param PARAM-TYPE-STRINGS     : Param type strings list.
@param PARAM-VARIABLE-STRINGS : Param name strings list.
"

  (when (or (jcs-is-current-major-mode-p "c++-mode")
            (jcs-is-current-major-mode-p "c-mode"))
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
    (while (>= param-index 0)
      (insert "\n")  ;; start from newline.
      (insert "* @")
      (insert jcs-cc-param-string)
      (when (not (equal jcs-cc-doc-show-typename nil))
        (jcs-insert-jsdoc-type (nth param-index param-type-strings)
                               jcs-cc-open-type-char
                               jcs-cc-close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs-cc-doc-after-value-type-char)
      (insert jcs-param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1- param-index)))

    ;; Lastly, process returns tag.
    (if (equal there-is-return t)
        (progn
          (if (not(string= return-type-string "void"))
              (progn
                (insert "\n")
                (insert "* @")
                (insert jcs-cc-return-string)
                (if (not (equal jcs-cc-doc-show-typename nil))
                    (jcs-insert-jsdoc-type return-type-string
                                           jcs-cc-open-type-char
                                           jcs-cc-close-type-char))
                (backward-delete-char 1)
                (if (not (equal jcs-cc-doc-show-typename nil))
                    (insert jcs-cc-doc-after-value-type-char)
                  (insert " "))
                (insert jcs-return-desc-string)
                (indent-for-tab-command)))))))

(defun jcs-java-mode-doc-string (meet-function-name
                                 keyword-strings
                                 datatype-name
                                 function-name-string
                                 there-is-return
                                 return-type-string
                                 param-type-strings
                                 param-variable-strings)
  "Insert `java-mode' doc string.

@param MEET-FUNCTION-NAME     : Meet the function name?
@param KEYWORD-STRINGS        : Keyword strings list.
@param DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
@param THERE-IS-RETURN        : There is return in this function?
@param FUNCTION-NAME-STRING   : Function name.
@param RETURN-TYPE-STRING     : String of the return type.
@param PARAM-TYPE-STRINGS     : Param type strings list.
@param PARAM-VARIABLE-STRINGS : Param name strings list.
"

  (when (or (jcs-is-current-major-mode-p "java-mode")
            (jcs-is-current-major-mode-p "jdee-mode"))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process param tag.
    (while (>= param-index 0)
      (insert "\n")  ;; start from newline.
      (insert "* @")
      (insert jcs-java-param-string)
      (when (not (equal jcs-java-doc-show-typename nil))
        (jcs-insert-jsdoc-type (nth param-index param-type-strings)
                               jcs-java-open-type-char
                               jcs-java-close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs-java-doc-after-value-type-char)
      (insert jcs-param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1- param-index)))

    ;; Lastly, process returns tag.
    (when (equal there-is-return t)
      (when (not(string= return-type-string "void"))
        (insert "\n")
        (insert "* @")
        (insert jcs-java-return-string)
        (when (not (equal jcs-java-doc-show-typename nil))
          (jcs-insert-jsdoc-type return-type-string
                                 jcs-java-open-type-char
                                 jcs-java-close-type-char))
        (backward-delete-char 1)
        (if (not (equal jcs-java-doc-show-typename nil))
            (insert jcs-java-doc-after-value-type-char)
          (insert " "))
        (insert jcs-return-desc-string)
        (indent-for-tab-command)))))

(defun jcs-js-mode-doc-string (meet-function-name
                               keyword-strings
                               datatype-name
                               function-name-string
                               there-is-return
                               return-type-string
                               param-type-strings
                               param-variable-strings)
  "Insert `js2-mode' doc string.

@param MEET-FUNCTION-NAME     : Meet the function name?
@param KEYWORD-STRINGS        : Keyword strings list.
@param DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
@param FUNCTION-NAME-STRING   : Function name.
@param THERE-IS-RETURN        : There is return in this function?
@param RETURN-TYPE-STRING     : String of the return type.
@param PARAM-TYPE-STRINGS     : Param type strings list.
@param PARAM-VARIABLE-STRINGS : Param name strings list.
"
  (when (or (jcs-is-current-major-mode-p "js2-mode"))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process param tag.
    (while (>= param-index 0)
      (insert "\n")  ;; start from newline.
      (insert "* @")
      (insert jcs-js-param-string)
      (when (not (equal jcs-js-doc-show-typename nil))
        (jcs-insert-jsdoc-type "typename"
                               jcs-js-open-type-char
                               jcs-js-close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs-js-doc-after-value-type-char)
      (insert jcs-param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1- param-index)))

    ;; Lastly, process returns tag.
    (when (equal there-is-return t)
      (when (not(string= return-type-string "void"))
        (insert "\n")
        (insert "* @")
        (insert jcs-js-return-string)
        (when (not (equal jcs-js-doc-show-typename nil))
          (jcs-insert-jsdoc-type return-type-string
                                 jcs-js-open-type-char
                                 jcs-js-close-type-char))
        (backward-delete-char 1)
        (if (not (equal jcs-js-doc-show-typename nil))
            (insert jcs-js-doc-after-value-type-char)
          (insert " "))
        (insert jcs-return-desc-string)
        (indent-for-tab-command)))))

(defun jcs-lua-mode-doc-string (meet-function-name
                                keyword-strings
                                datatype-name
                                function-name-string
                                there-is-return
                                return-type-string
                                param-type-strings
                                param-variable-strings)
  "Insert `lua-mode' doc string.

@param MEET-FUNCTION-NAME     : Meet the function name?
@param KEYWORD-STRINGS        : Keyword strings list.
@param DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
@param FUNCTION-NAME-STRING   : Function name.
@param THERE-IS-RETURN        : There is return in this function?
@param RETURN-TYPE-STRING     : String of the return type.
@param PARAM-TYPE-STRINGS     : Param type strings list.
@param PARAM-VARIABLE-STRINGS : Param name strings list.
"
  (when (or (jcs-is-current-major-mode-p "lua-mode"))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process param tag.
    (while (>= param-index 0)
      (insert "\n")  ;; start from newline.
      (insert "-- @")
      (insert jcs-lua-param-string)
      (when (not (equal jcs-lua-doc-show-typename nil))
        (jcs-insert-jsdoc-type "typename"
                               jcs-lua-open-type-char
                               jcs-lua-close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs-lua-doc-after-value-type-char)
      (insert jcs-param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1- param-index)))

    ;; Lastly, process returns tag.
    (when (equal there-is-return t)
      (when (not(string= return-type-string "void"))
        (insert "\n")
        (insert "-- @")
        (insert jcs-lua-return-string)
        (when (not (equal jcs-lua-doc-show-typename nil))
          (jcs-insert-jsdoc-type return-type-string
                                 jcs-lua-open-type-char
                                 jcs-lua-close-type-char))
        (backward-delete-char 1)
        (if (not (equal jcs-lua-doc-show-typename nil))
            (insert jcs-lua-doc-after-value-type-char)
          (insert " "))
        (insert jcs-return-desc-string)
        (indent-for-tab-command)))))

(defun jcs-py-mode-doc-string (meet-function-name
                               keyword-strings
                               datatype-name
                               function-name-string
                               there-is-return
                               return-type-string
                               param-type-strings
                               param-variable-strings)
  "Insert `python-mode' doc string.

@param MEET-FUNCTION-NAME     : Meet the function name?
@param KEYWORD-STRINGS        : Keyword strings list.
@param DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
@param FUNCTION-NAME-STRING   : Function name.
@param THERE-IS-RETURN        : There is return in this function?
@param RETURN-TYPE-STRING     : String of the return type.
@param PARAM-TYPE-STRINGS     : Param type strings list.
@param PARAM-VARIABLE-STRINGS : Param name strings list.
"
  (when (or (jcs-is-current-major-mode-p "python-mode"))
    ;; go back to comment line.
    (jcs-move-to-forward-a-char-recursive "\"")
    (jcs-move-to-forward-a-char-recursive "\"")
    (jcs-move-to-forward-a-char-recursive "\"")

    (if (= jcs-py-doc-string-version 1)
        (progn
          ;; OPTION(jenchieh): docstring option..
          (jcs-next-line)))
    (end-of-line)

    ;; Line breack between description and tags.
    (if (>= param-index 0)
        (insert "\n"))

    (while (>= param-index 0)
      (when (not (string= "self" (nth param-index param-variable-strings)))
        (insert "\n")  ;; start from newline.
        (insert "@")
        (insert jcs-py-param-string)
        (when (not (equal jcs-py-doc-show-typename nil))
          (jcs-insert-jsdoc-type "typename"
                                 jcs-py-open-type-char
                                 jcs-py-close-type-char))
        (insert (nth param-index param-variable-strings))
        (insert jcs-py-doc-after-value-type-char)
        (insert jcs-param-desc-string)

        ;; indent once.
        (indent-for-tab-command))

      ;; add up counter.
      (setq param-index (1- param-index)))

    ;; Lastly, process returns tag.
    (when (equal there-is-return t)
      (when (not(string= return-type-string "void"))
        (insert "\n")
        (insert "@")
        (insert jcs-py-return-string)
        (when (not (equal jcs-py-doc-show-typename nil))
          (jcs-insert-jsdoc-type return-type-string
                                 jcs-py-open-type-char
                                 jcs-py-close-type-char))
        (backward-delete-char 1)
        (if (not (equal jcs-py-doc-show-typename nil))
            (insert jcs-py-doc-after-value-type-char)
          (insert " "))
        (insert jcs-return-desc-string)
        (indent-for-tab-command)))))


(defun jcs-php-mode-doc-string (meet-function-name
                                keyword-strings
                                datatype-name
                                function-name-string
                                there-is-return
                                return-type-string
                                param-type-strings
                                param-variable-strings)
  "Insert `php-mode' doc string.

@param MEET-FUNCTION-NAME     : Meet the function name?
@param KEYWORD-STRINGS        : Keyword strings list.
@param DATATYPE-NAME          : Data type name, store keyword for
                               struct/class related.
@param FUNCTION-NAME-STRING   : Function name.
@param THERE-IS-RETURN        : There is return in this function?
@param RETURN-TYPE-STRING     : String of the return type.
@param PARAM-TYPE-STRINGS     : Param type strings list.
@param PARAM-VARIABLE-STRINGS : Param name strings list.
"
  (when (or (jcs-is-current-major-mode-p "php-mode")
            (jcs-is-current-major-mode-p "web-mode"))
    ;; go back to comment line.
    (jcs-previous-line)
    (jcs-previous-line)
    (end-of-line)

    ;; Process param tag.
    (while (>= param-index 0)
      (insert "\n")  ;; start from newline.
      (insert "* @")
      (insert jcs-js-param-string)
      (when (not (equal jcs-php-doc-show-typename nil))
        (jcs-insert-jsdoc-type "typename"
                               jcs-php-open-type-char
                               jcs-php-close-type-char))
      (insert (nth param-index param-variable-strings))
      (insert jcs-php-doc-after-value-type-char)
      (insert jcs-param-desc-string)

      ;; indent once.
      (indent-for-tab-command)

      ;; add up counter.
      (setq param-index (1- param-index)))

    ;; Lastly, process returns tag.
    (when (equal there-is-return t)
      (when (not(string= return-type-string "void"))
        (insert "\n")
        (insert "* @")
        (insert jcs-php-return-string)
        (if (not (equal jcs-php-doc-show-typename nil))
            (jcs-insert-jsdoc-type return-type-string
                                   jcs-php-open-type-char
                                   jcs-php-close-type-char))
        (backward-delete-char 1)
        (if (not (equal jcs-php-doc-show-typename nil))
            (insert jcs-php-doc-after-value-type-char)
          (insert " "))
        (insert jcs-return-desc-string)
        (indent-for-tab-command)))))


(defun jcs-insert-jsdoc-type (type-name open-char close-char)
  "Insert the curly bracket part.

@param TYPE-NAME : type name string.
@param OPEN-CHAR : opening character.
@param CLOSE-CHAR : closing character."
  (interactive)
  (insert open-char)
  (insert type-name)
  (insert close-char))


;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defvar jcs-oop-font-lock-missing-strict-modes '(c-mode)
  "Modes to fixed variable font lock missing face.")

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
      jcs-oop-font-lock-missing-strict-modes)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defvar jcs-oop-font-lock-missing-modes '(lua-mode
                                          php-mode
                                          python-mode)
  "Modes to fixed variable font lock missing face.")

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("(,*\\([a-zA-Z_$0-9 \t]*\\)[,)]" 1 'font-lock-variable-name-face t)
           (",\\([a-zA-Z_$0-9, \t]*\\)," 1 'font-lock-variable-name-face t)
           ("\\([a-zA-Z_$0-9 \t]*\\)[)]" 1 'font-lock-variable-name-face t)
           ;; For line break parameter declaration.
           ("^[ \t]* \\([a-zA-Z_$0-9,]*\\)[ \t]*[,)]" 1 'font-lock-variable-name-face t)
           )'end))
      jcs-oop-font-lock-missing-modes)
