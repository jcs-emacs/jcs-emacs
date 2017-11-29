;; This is the start of jcs-oop-func.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-oop-func.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Tue Nov 21 13:51:49 EST 2017>
;; Time-stamp: <2017-11-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-oop-func is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-oop-func is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


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
                                  web-mode))

(defface jcs-oop-tag-face
  '((t (:foreground "#38EFCA")))
  "Highlight OOP tag.")
(defvar jcs-oop-tag-face 'jcs-oop-tag-face)

(defface jcs-oop-type-face
  '((t (:foreground "SteelBlue")))
  "Highlight OOP type.")
(defvar jcs-oop-type-face 'jcs-oop-type-face)

(defface jcs-oop-value-face
  '((t (:foreground "PeachPuff3")))
  "Highlight OOP value.")
(defvar jcs-oop-value-face 'jcs-oop-value-face)


;; STUDY(jenchieh): https://stackoverflow.com/questions/5073930/how-to-color-at-symbol-in-emacs
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\(?:^\\|\\s-\\)\\(@[a-zA-Z0-9_]*\\)" 1 'jcs-oop-tag-face t)
           ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
           ;; OPTION(jenchieh): Highlight curly bracket.
           ("@[a-zA-Z0-9_].*\\(?:^\\|\\s-\\)\\({.*.}\\)" 1 'jcs-oop-type-face t)
           ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           ;; OR(jenchieh):
           ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           ;; OPTION(jenchieh): Don't highlight curly bracket.
           ;;("@[a-zA-Z0-9_].*\\(?:^\\|\\s-\\){\\(.*.\\)}" 1 'jcs-oop-type-face t)
           ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
           ("@[a-zA-Z0-9_].*}.\\([a-zA-Z0-9_]*\\).[-\|:]" 1 'jcs-oop-value-face t)
           ("@[a-zA-Z0-9_]*.\\([a-zA-Z0-9_]*\\).*[{\|:\|-]" 1 'jcs-oop-value-face t)
           )))
      jcs-oop-highlight-modes)


;; DESCRIPTION(jenchieh): Change this variable for customize
;; delimiter in OOP doc.
(defvar jcs-oop-doc-delimiter-char " : "
  "Default OOP delimiter character.")

(defvar jcs-java-doc-show-curly-type nil
  "Show the typename betweeen the open curly bracket and close curly bracket in Java mode.")

(defvar jcs-cc-doc-show-curly-type t
  "Show the typename betweeen the open curly bracket and close curly bracket in C/C++ mode.")

(defvar jcs-js-doc-show-curly-type t
  "Show the typename betweeen the open curly bracket and close curly bracket in JavaScript mode.")

(defvar jcs-lua-doc-show-curly-type t
  "Show the typename betweeen the open curly bracket and close curly bracket in Lua mode.")

(defvar jcs-py-doc-show-curly-type t
  "Show the typename betweeen the open curly bracket and close curly bracket in Python mode.")


(defun jcs-insert-comment-style-by-current-line ()
  "Read the current line and insert by reading the need from
the input line."
  (interactive)
  (let ((keyword-strings '())
        (datatype-name "")
        (meet-function-name nil)
        (function-name-string "")
        (param-type-strings '())  ;; param type string list.
        (param-variable-strings '())  ;; param name string list.
        (there-is-return nil)
        (return-type-string ""))
    (save-excursion
      (if (not (current-line-empty-p))
          (progn
            (end-of-line)

            (let ((end-line-point (1- (point)))
                  (word-index 0))

              (beginning-of-line)

              (while (< (point) end-line-point)
                (if (not (= word-index 0))
                    (forward-word))
                (forward-word)
                (backward-char 1)
                (setq word-index (1+ word-index))

                ;; Make sure only process current/one line.
                (if (< (point) end-line-point)
                    (progn
                      (let ((current-point-face(jcs-get-current-point-face) ))

                        ;; NOTE(jenchieh): If there is multiple faces at
                        ;; a point, it will return a list instead of
                        ;; string. Just get the first element which is
                        ;; usually the foreground face.
                        (if (listp current-point-face)
                            (setq current-point-face (nth 0 current-point-face)))

                        ;; NOTE(jenchieh): Store all the keyword name.
                        (if (or (string= current-point-face "font-lock-keyword-face")
                                (string= current-point-face "font-lock-preprocessor-face"))
                            (add-to-list 'keyword-strings (thing-at-point 'word)))

                        ;; NOTE(jenchieh): Check if meet the function name.
                        (if (string= current-point-face "font-lock-function-name-face")
                            (progn
                              (setq function-name-string (thing-at-point 'word))
                              (setq meet-function-name t)))

                        ;; NOTE(jenchieh): Store all the type name. (include return type name)
                        (if (string= current-point-face "font-lock-type-face")
                            (progn
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
                                    (setq param-type-strings (append param-type-strings temp-list))))
                                )))

                        ;; NOTE(jenchieh): Store all the variables name.
                        (if (or (string= current-point-face "font-lock-variable-name-face")
                                (string= current-point-face 'js2-function-param))
                            (progn
                              (add-to-list 'param-variable-strings (thing-at-point 'word))
                              ))
                        ))))


              ))))
    ;; Insert document comment string.
    (jcs-insert-doc-comment-string meet-function-name
                                   keyword-strings
                                   datatype-name
                                   function-name-string
                                   there-is-return
                                   return-type-string
                                   param-type-strings
                                   param-variable-strings)
    ))

(defun jcs-insert-doc-comment-string (meet-function-name
                                      keyword-strings
                                      datatype-name
                                      function-name-string
                                      there-is-return
                                      return-type-string
                                      param-type-strings
                                      param-variable-strings)
  "Insert document comment style.

@param meet-function-name     : Meet the function name?
@param keyword-strings        : Keyword strings list.
@param datatype-name          : Data type name, store keyword for
                               struct/class related.
@param function-name-string   : Function name.
@param there-is-return        : There is return in this function?
@param return-type-string     : String of the return type.
@param param-type-strings     : Param type strings list.
@param param-variable-strings : Param name strings list.
"
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

            (if (jcs-is-current-major-mode-p "csharp-mode")
                (progn
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
                    (setq param-index (1- param-index))
                    )

                  ;; Lastly, process returns tag.
                  (if (equal there-is-return t)
                      (progn
                        (if (not(string= return-type-string "void"))
                            (progn
                              (insert "\n")
                              (insert "/// <returns></returns>")
                              (indent-for-tab-command)))
                        ))
                  ))

            (if (or (jcs-is-current-major-mode-p "c++-mode")
                    (jcs-is-current-major-mode-p "c-mode"))
                (progn
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
                    (insert "* @param ")
                    (if (not (equal jcs-cc-doc-show-curly-type nil))
                        (progn
                          (jcs-insert-jsdoc-type (nth param-index param-type-strings))
                          ))
                    (insert (nth param-index param-variable-strings))
                    (insert jcs-oop-doc-delimiter-char)
                    (insert "Param desc here..")

                    ;; indent once.
                    (indent-for-tab-command)

                    ;; add up counter.
                    (setq param-index (1- param-index))
                    )

                  ;; Lastly, process returns tag.
                  (if (equal there-is-return t)
                      (progn
                        (if (not(string= return-type-string "void"))
                            (progn
                              (insert "\n")
                              (insert "* @returns ")
                              (if (not (equal jcs-cc-doc-show-curly-type nil))
                                  (jcs-insert-jsdoc-type return-type-string)
                                (backward-delete-char 1))
                              (insert jcs-oop-doc-delimiter-char)
                              (insert "Returns description here..")
                              (indent-for-tab-command)))
                        ))
                  ))

            (if (or (jcs-is-current-major-mode-p "java-mode")
                    (jcs-is-current-major-mode-p "jdee-mode"))
                (progn
                  ;; go back to comment line.
                  (jcs-previous-line)
                  (jcs-previous-line)
                  (end-of-line)

                  ;; Process param tag.
                  (while (>= param-index 0)
                    (insert "\n")  ;; start from newline.
                    (insert "* @param ")
                    (if (not (equal jcs-java-doc-show-curly-type nil))
                        (progn
                          (jcs-insert-jsdoc-type (nth param-index param-type-strings))
                          ))
                    (insert (nth param-index param-variable-strings))
                    (insert jcs-oop-doc-delimiter-char)
                    (insert "Param desc here..")

                    ;; indent once.
                    (indent-for-tab-command)

                    ;; add up counter.
                    (setq param-index (1- param-index))
                    )

                  ;; Lastly, process returns tag.
                  (if (equal there-is-return t)
                      (progn
                        (if (not(string= return-type-string "void"))
                            (progn
                              (insert "\n")
                              (insert "* @returns ")
                              (if (not (equal jcs-java-doc-show-curly-type nil))
                                    (jcs-insert-jsdoc-type return-type-string)
                                (backward-delete-char 1))
                              (insert jcs-oop-doc-delimiter-char)
                              (insert "Returns description here..")
                              (indent-for-tab-command)))
                        ))
                  ))

            (if (or (jcs-is-current-major-mode-p "js2-mode"))
                (progn
                  ;; go back to comment line.
                  (jcs-previous-line)
                  (jcs-previous-line)
                  (end-of-line)

                  ;; Process param tag.
                  (while (>= param-index 0)
                    (insert "\n")  ;; start from newline.
                    (insert "* @param ")
                    (if (not (equal jcs-js-doc-show-curly-type nil))
                        (progn
                          (jcs-insert-jsdoc-type "typename")
                          ))
                    (insert (nth param-index param-variable-strings))
                    (insert jcs-oop-doc-delimiter-char)
                    (insert "Param desc here..")

                    ;; indent once.
                    (indent-for-tab-command)

                    ;; add up counter.
                    (setq param-index (1- param-index))
                    )

                  ;; Lastly, process returns tag.
                  (if (equal there-is-return t)
                      (progn
                        (if (not(string= return-type-string "void"))
                            (progn
                              (insert "\n")
                              (insert "* @returns ")
                              (if (not (equal jcs-js-doc-show-curly-type nil))
                                  (jcs-insert-jsdoc-type return-type-string)
                                (backward-delete-char 1))
                              (insert jcs-oop-doc-delimiter-char)
                              (insert "Returns description here..")
                              (indent-for-tab-command)))
                        ))
                  ))

            (if (or (jcs-is-current-major-mode-p "lua-mode"))
                (progn
                  ;; go back to comment line.
                  (jcs-previous-line)
                  (jcs-previous-line)
                  (end-of-line)

                  ;; Process param tag.
                  (while (>= param-index 0)
                    (insert "\n")  ;; start from newline.
                    (insert "-- @param ")
                    (if (not (equal jcs-lua-doc-show-curly-type nil))
                        (progn
                          (jcs-insert-jsdoc-type (nth param-index param-variable-strings))
                          ))
                    (insert jcs-oop-doc-delimiter-char)
                    (insert "Param desc here..")

                    ;; indent once.
                    (indent-for-tab-command)

                    ;; add up counter.
                    (setq param-index (1- param-index))
                    )

                  ;; Lastly, process returns tag.
                  (if (equal there-is-return t)
                      (progn
                        (if (not(string= return-type-string "void"))
                            (progn
                              (insert "\n")
                              (insert "-- @returns ")
                              (if (not (equal jcs-lua-doc-show-curly-type nil))
                                  (jcs-insert-jsdoc-type return-type-string)
                                (backward-delete-char 1))
                              (insert jcs-oop-doc-delimiter-char)
                              (insert "Returns description here..")
                              (indent-for-tab-command)))
                        ))
                  ))

            (if (or (jcs-is-current-major-mode-p "python-mode"))
                (progn
                  ;; go back to comment line.
                  (jcs-next-line)
                  (jcs-next-line)
                  (end-of-line)

                  ;; TODO(jenchieh): implement into python mode.
                  ))
            ))
      ;; NOTE(jenchieh): Design object comment document string.
      (progn

        (if (jcs-is-current-major-mode-p "csharp-mode")
            (progn
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
                       )))
              ))

        (if (or (jcs-is-current-major-mode-p "c++-mode")
                (jcs-is-current-major-mode-p "c-mode"))
            (progn
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
                       (insert "* @brief Class description here..")
                       (indent-for-tab-command)
                       ))
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
                       (insert "* @brief Struct description here..")
                       (indent-for-tab-command)
                       ))
                    ((or (jcs-is-in-list-string keyword-strings "define")
                         (jcs-is-in-list-string keyword-strings "#define"))
                     (progn
                       ;; go back to comment line.
                       (jcs-previous-line)
                       (jcs-previous-line)
                       (end-of-line)

                       ;; Process class tag.
                       (insert "@def ")
                       (insert (nth 0 param-variable-strings))
                       (indent-for-tab-command)

                       ;; Process brief tag.
                       (insert "\n")
                       (insert "* @brief Define description here..")
                       (indent-for-tab-command)
                       ))
                    )
              ))

        (if (or (jcs-is-current-major-mode-p "java-mode")
                (jcs-is-current-major-mode-p "jdee-mode"))
            (progn
              (cond ((jcs-is-in-list-string keyword-strings "class")
                     (progn
                       ;; STUDY(jenchieh): Don't think that java
                       ;; doc need one..
                       ))
                    ((jcs-is-in-list-string keyword-strings "interface")
                     (progn
                       ;; STUDY(jenchieh): Don't think that java
                       ;; doc need one..
                       )))
              ))

        (if (or (jcs-is-current-major-mode-p "js2-mode"))
            (progn
              (cond ((jcs-is-in-list-string keyword-strings "class")
                     (progn
                       ;; STUDY(jenchieh): Don't know if javascript
                       ;; need one..
                       )))
              ))

        (if (or (jcs-is-current-major-mode-p "lua-mode"))
            (progn
              ;; NOTE(jenchieh): I don't think Lua have any keywords...
              ))

        (if (or (jcs-is-current-major-mode-p "python-mode"))
            (progn
              (cond ((jcs-is-in-list-string keyword-strings "class")
                     (progn
                       ;; TODO(jenchieh): implement into python mode.
                       )))
              ))
        ))))

(defun jcs-insert-jsdoc-type (type-name)
  "Insert the curly bracket part. { type-name }"
  (interactive)
  (insert "{ ")
  (insert type-name)
  (insert " } ")
  )

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-oop-func.el file
