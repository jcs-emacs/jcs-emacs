;;; jcs-edit.el --- When editing the file  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Undo / Redo" )
;;

(defun jcs-undo-tree-visualize ()
  "Call `undo-tree-visualize' only in window that has higher height."
  (save-window-excursion (undo-tree-visualize))
  (with-selected-window (get-largest-window nil nil t)
    (switch-to-buffer undo-tree-visualizer-buffer-name)
    (jcs-recenter-top-bottom 'middle)
    (fill-page-if-unfill)))

(defun jcs--undo-or-redo (ud)
  "Do undo or redo base on UD.
If UD is non-nil, do undo.  If UD is nil, do redo."
  (require 'undo-tree)
  (if (not undo-tree-mode)
      (call-interactively #'undo)  ; undo/redo are the same command
    ;; NOTE: If we do jumped to the `undo-tree-visualizer-buffer-name'
    ;; buffer, then we use `undo-tree-visualize-redo' instead of
    ;; `undo-tree-redo'. Because directly called `undo-tree-visualize-redo'
    ;; key is way faster than `undo-tree-redo' key.
    (jcs-if-buffer-window undo-tree-visualizer-buffer-name
        (if ud (undo-tree-visualize-undo) (undo-tree-visualize-redo))
      (if ud (undo-tree-undo) (undo-tree-redo))
      (jcs-undo-tree-visualize))))

(defun jcs-undo () "Undo key." (interactive) (jcs--undo-or-redo t))
(defun jcs-redo () "Redo key." (interactive) (jcs--undo-or-redo nil))

;;
;; (@* "Organize Imports" )
;;

(defun jcs-organize-imports ()
  "Organize imports code."
  (interactive)
  (cond
   ((ignore-errors (lsp-organize-imports)))  ; first try lsp
   (t (cl-case major-mode
        (`java-mode
         (jcs-java-insert-package-src)  ; first organize package declaration
         (organize-imports-java-do-imports))))))

;;
;; (@* "Indentation" )
;;

(jcs-advice-add 'indent-line-to :before (indent-control-ensure-tab-width))

;;
;; (@* "Backspace" )
;;

(defun jcs-real-backspace ()
  "Just backspace a char."
  (interactive)
  (jcs-electric-backspace))

(defun jcs-smart-backspace ()
  "Smart backspace."
  (interactive)
  (or (and (jcs-is-infront-first-char-at-line-p) (not (jcs-beginning-of-line-p))
           (not (use-region-p))
           (jcs-backward-delete-spaces-by-indent-level))
      (jcs-real-backspace)))

;;
;; (@* "Delete" )
;;

(defun jcs-real-delete ()
  "Just delete a char."
  (interactive)
  (jcs-electric-delete))

(defun jcs-smart-delete ()
  "Smart backspace."
  (interactive)
  (or (and (not (eobp))
           (jcs-is-infront-first-char-at-line-p (1+ (point)))
           (jcs-forward-delete-spaces-by-indent-level))
      (jcs-real-delete)))

;;
;; (@* "Return" )
;;

(defun jcs--newline--advice-around (fnc &rest args)
  "Advice execute around function `newline'."
  (when (jcs-current-line-totally-empty-p) (indent-for-tab-command))
  (let ((ln-cur (buffer-substring (line-beginning-position) (point))))
    (apply fnc args)
    (save-excursion
      (forward-line -1)
      (when (jcs-current-line-totally-empty-p) (insert ln-cur)))))
(advice-add 'newline :around #'jcs--newline--advice-around)

(defun jcs-ctrl-return-key ()
  "Global Ctrl-Return key."
  (interactive)
  ;;;
  ;; Priority
  ;;
  ;; ATTENTION: all the function in the priority function
  ;; list must all have error handling. Or else this the
  ;; priority chain will break.
  ;;
  ;; 1. `project-abbrev-complete-word'
  ;; 2. `yas-expand'
  ;; 3. `goto-address-at-point'
  ;;
  (cond ((ignore-errors (call-interactively #'project-abbrev-complete-word)))
        ((ignore-errors (call-interactively #'yas-expand)))
        ((ffap-url-at-point) (call-interactively #'goto-address-at-point))
        (t
         (cl-case major-mode
           (`org-mode (call-interactively #'org-todo))
           (t (call-interactively (key-binding (kbd "RET"))))))))

;;
;; (@* "Space" )
;;

(defun jcs-real-space ()
  "Just insert a space."
  (interactive)
  (insert " "))

(defun jcs-smart-space ()
  "Smart way of inserting space."
  (interactive)
  (if (jcs-current-line-empty-p)
      (let ((pt (point)))
        (ignore-errors (indent-for-tab-command))
        (when (= pt (point)) (jcs-real-space)))
    (if (or (jcs-is-infront-first-char-at-line-p) (jcs-beginning-of-line-p))
        (jcs-insert-spaces-by-indent-level)
      (jcs-real-space))))

;;
;; (@* "Yank" )
;;

(defun jcs-smart-yank ()
  "Yank and then indent region."
  (interactive)
  (jcs-mute-apply
    (jcs-delete-region)
    (let ((reg-beg (point)))
      (call-interactively #'yank)
      (ignore-errors (indent-region reg-beg (point))))))

;;
;; (@* "Tab" )
;;

(defun jcs-tab-key ()
  "Global TAB key."
  (interactive)
  (if (use-region-p)
      (jcs-lines-in-region
       (lambda ()
         (back-to-indentation)
         (jcs-insert-spaces-by-indent-level)))
    (unless (ignore-errors (call-interactively #'yas-expand))
      (if (company--active-p)
          (call-interactively #'company-complete-selection)
        (if (jcs-current-line-empty-p)
            (let ((pt (point)))
              (indent-for-tab-command)
              (when (= pt (point)) (jcs-insert-spaces-by-indent-level)))
          (jcs-insert-spaces-by-indent-level))))))

(defun jcs-shift-tab-key ()
  "Global Shift+TAB key."
  (interactive)
  (if (use-region-p)
      (jcs-lines-in-region
       (lambda ()
         (back-to-indentation)
         (let (delete-active-region)
           (jcs-backward-delete-spaces-by-indent-level))))
    (unless (ignore-errors (call-interactively #'yas-expand))
      (if (company--active-p)
          (call-interactively #'company-complete-selection)
        (if (jcs-current-line-empty-p)
            (let ((pt (point)))
              (indent-for-tab-command)
              (when (= pt (point)) (jcs-backward-delete-spaces-by-indent-level)))
          (jcs-backward-delete-spaces-by-indent-level))))))

;;
;; (@* "Overwrite" )
;;

(jcs-add-hook 'overwrite-mode-hook
  (require 'multiple-cursors)
  (electric-cursor-mode 1)
  (cond (overwrite-mode
         (set-face-attribute 'mc/cursor-face nil :underline t :inverse-video nil))
        (t (set-face-attribute 'mc/cursor-face nil :underline nil :inverse-video t))))

;;
;; (@* "Kill Line" )
;;

(defun jcs-kill-whole-line ()
  "Deletes a line, but does not put it in the `kill-ring'."
  (interactive)
  ;; SOURCE: http://ergoemacs.org/emacs/emacs_kill-ring.html
  (let (kill-ring)
    (if (use-region-p) (jcs-delete-region)
      (company-abort)
      ;; Record down the column before killing the whole line.
      (let ((before-column-num (current-column)))
        ;; Do kill the whole line!
        (delete-region (line-beginning-position)
                       (if (= (line-number-at-pos (point)) (line-number-at-pos (point-max)))
                           (line-end-position)
                         (1+ (line-end-position))))
        ;; Goto the same column as before we do the killing the whole line
        ;; operations above.
        (move-to-column before-column-num)))))

(defun jcs-backward-kill-line (arg)
  "Kill ARG lines backward, but does not put it in the `kill-ring'."
  (interactive "p")
  (kill-line (- 1 arg))
  (setq kill-ring (cdr kill-ring)))

(defun jcs-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With ARG, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun jcs-backward-delete-word (arg)
  "Backward deleteing ARG words."
  (interactive "p")
  (if (use-region-p) (jcs-delete-region) (jcs-delete-word (- arg))))

(defun jcs-forward-delete-word (arg)
  "Forward deleteing ARG words."
  (interactive "p")
  (if (use-region-p) (jcs-delete-region) (jcs-delete-word (+ arg))))

(defun jcs-smart-backward-delete-word ()
  "Backward deleteing ARG words in the smart way."
  (interactive)
  (if (use-region-p) (jcs-delete-region)
    (let ((start-pt -1) (end-pt (point)) (start-ln-end-pt -1))
      (save-excursion
        (jcs-smart-backward-word)
        (setq start-pt (point)
              start-ln-end-pt (line-end-position)))
      (unless (= (line-number-at-pos start-pt) (line-number-at-pos end-pt))
        (setq start-pt start-ln-end-pt))
      (delete-region start-pt end-pt))))

(defun jcs-smart-forward-delete-word ()
  "Forward deleteing ARG words in the smart way."
  (interactive)
  (if (use-region-p) (jcs-delete-region)
    (let ((start-pt (point)) (end-pt -1) (end-ln-start-pt -1))
      (save-excursion
        (jcs-smart-forward-word)
        (setq end-pt (point)
              end-ln-start-pt (line-beginning-position)))
      (unless (= (line-number-at-pos start-pt) (line-number-at-pos end-pt))
        (setq end-pt end-ln-start-pt))
      (delete-region start-pt end-pt))))

(defun jcs-backward-kill-word-capital ()
  "Backward delete the word unitl the word is capital."
  (interactive)
  (if (use-region-p) (jcs-delete-region)
    (let ((start-pt -1) (end-pt (point)) (start-ln-end-pt -1))
      (save-excursion
        (jcs-backward-word-capital)
        (setq start-pt (point)
              start-ln-end-pt (line-end-position)))
      (unless (= (line-number-at-pos start-pt) (line-number-at-pos end-pt))
        (setq start-pt start-ln-end-pt))
      (delete-region start-pt end-pt))))

(defun jcs-forward-kill-word-capital ()
  "Forward delete the word unitl the word is capital."
  (interactive)
  (if (use-region-p) (jcs-delete-region)
    (let ((start-pt (point)) (end-pt -1) (end-ln-start-pt -1))
      (save-excursion
        (jcs-forward-word-capital)
        (setq end-pt (point)
              end-ln-start-pt (line-beginning-position)))
      (unless (= (line-number-at-pos start-pt) (line-number-at-pos end-pt))
        (setq end-pt end-ln-start-pt))
      (delete-region start-pt end-pt))))

(defun jcs-duplicate-line ()
  "Duplicate the line."
  (interactive)
  (let ((cur-col (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (forward-line 1)
    (yank)
    (move-to-column cur-col)))

;;
;; (@* "Format File" )
;;

(defun jcs-format-document ()
  "Format current document."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun jcs-format-region-or-document ()
  "Format the document if there are no region apply."
  (interactive)
  (if (use-region-p) (indent-region (region-beginning) (region-end))
    (jcs-format-document)))

(defun jcs-align-region-by-points (regexp pnt-min pnt-max)
  "Align current selected region with REGEXP, PNT-MIN and PNT-MAX."
  (interactive)
  (align pnt-min pnt-max)
  (align-regexp pnt-min pnt-max regexp 1 1 t))

(defun jcs-align-region (regexp)
  "Align current selected region REGEXP."
  (interactive)
  (jcs-align-region-by-points regexp (region-beginning) (region-end))
  (deactivate-mark))

(defun jcs-align-document (regexp)
  "Align current document with REGEXP."
  (interactive)
  (jcs-align-region-by-points regexp (point-min) (point-max)))

(defun jcs-align-region-or-document ()
  "Either align the region or document depend on if there is region selected."
  (interactive)
  (save-excursion
    (let (;; NOTE: this is the most common one.
          ;; Compatible to all programming languages use equal sign to assign value.
          (align-regexp-string-code
           (cl-case major-mode
             (`nasm-mode "\\(\\s-*\\)equ ")
             (`go-mode "\\(\\s-*\\) := ")
             ((or lisp-mode lisp-interaction-mode emacs-lisp-mode) "\\(\\s-*\\)[.]")
             (t "\\(\\s-*\\)[=]")))
          ;; NOTE: Default support `//' and `/**/' comment symbols.
          (align-regexp-string-comment
           (cl-case major-mode
             (`nasm-mode "\\(\\s-*\\)               [;]")
             (t "\\(\\s-*\\) /[/*]")))
          (bound (jcs-region-bound)))
      ;; Align code segment
      (if (use-region-p)
          (jcs-align-region align-regexp-string-code)
        (jcs-align-document align-regexp-string-code))
      ;; Align comment segment
      (jcs-align-region-by-points align-regexp-string-comment (car bound) (cdr bound)))))

(defun jcs-align-repeat (regexp)
  "Repeat alignment with respect to the given REGEXP."
  (interactive "r\nsAlign regexp: ")
  (let ((bound (jcs-region-bound)))
    (align-regexp (car bound) (cdr bound) (concat "\\(\\s-*\\)" regexp) 1 1 t)))

;;
;; (@* "Line Ending" )
;;

(defun jcs-remove-control-M ()
  "Remove ^M at end of line in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward (concat (char-to-string 13) "$") (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match ""))
        (message "%d ^M removed from buffer." remove-count)))))

;;
;; (@* "Save Buffer" )
;;

(jcs-advice-add 'save-buffer :before
  (jcs-funcall-fboundp #'company-abort)
  ;; Delete trailing whitespaces execpt the current line
  (when whitespace-cleanup-mode
    (whitespace-cleanup-region (point-min) (line-beginning-position))
    (whitespace-cleanup-region (line-end-position) (point-max)))
  (when jcs-on-save-remove-control-M (jcs-mute-apply (jcs-remove-control-M))))

(jcs-advice-add 'save-buffer :after
  (jcs-funcall-fboundp #'undo-tree-kill-visualizer)
  (setq jcs-created-parent-dir-path nil))

(defun jcs-save-all-buffers ()
  "Save all buffers currently opened."
  (interactive)
  (let (saved-lst)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when-let ((result
                    (ignore-errors
                      (jcs-mute-apply
                        (call-interactively (key-binding (kbd "C-s")))))))
          (when (ignore-errors (string-match-p "Wrote file" result))
            (push (buffer-file-name) saved-lst)))))
    (unless save-silently
      (let ((len (length saved-lst))
            (info-str (mapconcat (lambda (buf) (format "`%s`" buf)) saved-lst "\n ")))
        (pcase len
          (0 (message "[INFO] (No buffers need to be saved)"))
          (1 (message "[INFO] %s buffer saved:\n %s" len info-str))
          (_ (message "[INFO] All %s buffers are saved:\n %s" len info-str)))))))

(defun jcs-save-buffer ()
  "Save buffer wrapper."
  (interactive)
  (cond
   ((not (buffer-file-name))
    (user-error "[WARNING] Can't save with invalid filename: %s" (buffer-name)))
   (buffer-read-only
    (user-error "[WARNING] Can't save read-only file: %s" buffer-read-only))
   (t
    (let ((readable (file-readable-p (buffer-file-name))))
      (jcs-no-log-apply (call-interactively #'save-buffer))
      (unless readable (jcs--safe-lsp-active))))))

;;
;; (@* "Find file" )
;;

(defun jcs-same-file-other-window ()
  "This will allow us open the same file in another window."
  (interactive)
  (switch-to-buffer-other-window (current-buffer)))

;;
;; (@* "Rename file" )
;;

(defun jcs-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  ;; SOURCE: https://emacs.stackexchange.com/questions/2849/save-current-file-with-a-slightly-different-name
  ;; URL: http://www.whattheemacsd.com/
  (let ((name (buffer-name)) (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((new-name (read-file-name "New name: " filename))
             (new-file-name (file-name-nondirectory new-name)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (if (string= name new-file-name)
              (message "Filename doesn't change, `%s`" name)
            (message "Rename file `%s` to `%s`" name new-file-name)))))))

;;
;; (@* "Kill Buffer" )
;;

(defconst jcs-must-kill-buffer-list
  `(,(regexp-quote jcs-message-buffer-name)
    "[*]compilation" "[*]output")
  "List of buffer name that must be killed when maybe kill; unless it shows up
in multiple windows.")

(defun jcs-bury-diminished-buffer ()
  "Bury the diminished buffer."
  (when (and diminish-buffer-mode
             (jcs-contain-list-type-str (jcs-buffer-name-or-buffer-file-name)
                                        diminish-buffer-list
                                        'regex))
    (jcs-bury-buffer)))

(defun jcs-bury-buffer ()
  "Bury this buffer."
  (interactive)
  (let ((bn (jcs-buffer-name-or-buffer-file-name)))
    (bury-buffer)
    (when (or (jcs-buffer-menu-p)
              (string= bn (jcs-buffer-name-or-buffer-file-name)))
      (bury-buffer)))
  ;; If something that I doesn't want to see, bury it.
  ;; For instance, any `*helm-' buffers.
  (jcs-bury-diminished-buffer))

(defun jcs--kill-this-buffer--advice-around (fnc &rest args)
  "Advice execute around command `kill-this-buffer' with FNC and ARGS."
  (require 'undo-tree)
  (let ((killed-buffer (current-buffer)) undoing-p)
    (jcs-with-current-buffer undo-tree-visualizer-buffer-name
      (setq undoing-p (eq undo-tree-visualizer-parent-buffer killed-buffer)))
    (apply fnc args)
    (undo-tree-kill-visualizer)))
(advice-add 'kill-this-buffer :around #'jcs--kill-this-buffer--advice-around)

(defun jcs-kill-this-buffer ()
  "Kill this buffer."
  (interactive)
  (when jcs-created-parent-dir-path  ; Remove virtual parent directory.
    (let* ((topest-dir (nth 0 (f-split jcs-created-parent-dir-path)))
           (create-dir (s-replace jcs-created-parent-dir-path "" default-directory))
           (del-path (f-slash (concat create-dir topest-dir))))
      (delete-directory del-path)
      (message "[INFO] Remove parent directory that were virtual => '%s'" del-path)))
  (when (and (featurep 'lsp-mode) (jcs--lsp-connected-p)) (lsp-disconnect))
  (kill-this-buffer)
  (jcs-project--track-open-projects)
  ;; If still in the buffer menu, try switch to the previous buffer.
  (when (jcs-buffer-menu-p) (switch-to-prev-buffer)))

(defun jcs-maybe-kill-this-buffer (&optional ecp-same)
  "Kill buffer if the current buffer is the only shown in one window.
Otherwise just switch to the previous buffer to keep the buffer.

If  optional argument ECP-SAME is non-nil then it allows same buffer on the
other window."
  (interactive)
  (let ((must-kill-buf-p
         (jcs-contain-list-type-str (buffer-name) jcs-must-kill-buffer-list 'regex))
        (shown-multiple-p (jcs-buffer-shown-in-multiple-window-p (buffer-name) 'strict))
        (cur-buf (current-buffer))
        is-killed)
    (if (or shown-multiple-p
            (and (jcs-virtual-buffer-p) (not (jcs-invalid-buffer-p))))
        (progn
          (jcs-bury-buffer)
          (when (and must-kill-buf-p (not shown-multiple-p))
            (setq is-killed t)
            (with-current-buffer cur-buf (kill-this-buffer))))
      (jcs-kill-this-buffer)
      (setq is-killed t)

      ;; NOTE: After kill the buffer, if the buffer appear in multiple windows
      ;; then we do switch to previous buffer again. Hence, it will not show
      ;; repeated buffer at the same time in different windows.
      (when (and (not ecp-same)
                 (jcs-buffer-shown-in-multiple-window-p (buffer-name) 'strict))
        (jcs-bury-buffer)

        ;; If is something from default Emacs's buffer, switch back to previous
        ;; buffer once again.
        ;;
        ;; This will solve if there is only one file opened, and switch to none
        ;; sense buffer issue.
        ;;
        ;; None sense buffer or Emacs's default buffer is
        ;;   -> *GNU Emacs*
        ;;   -> *scratch*
        ;;   , etc.
        (when (and (not (jcs-valid-buffer-p)) (>= (jcs-valid-buffers-count) 2))
          (jcs-switch-to-next-valid-buffer))))
    ;; If something that I doesn't want to see, bury it.
    ;; For instance, any `*helm-' buffers.
    (jcs-bury-diminished-buffer)
    is-killed))

(defun jcs-reopen-this-buffer ()
  "Kill the current buffer and open it again."
  (interactive)
  (when-let ((current-bfn (buffer-file-name)))
    (jcs-no-log-apply
      (jcs-save-window-excursion (jcs-kill-this-buffer))
      (undo-tree-kill-visualizer)
      (message "%s[INFO] Reopened file => '%s'"
               (if (current-message) (concat (current-message) "\n\n") "")
               current-bfn))))

;;
;; (@* "Electric Pair" )
;;

(defun jcs-get-open-pair-char (c)
  "Get the open pairing character from C."
  (pcase c
    ("\"" '("\""))
    ("'" '("'" "`"))
    (")" '("("))
    ("]" '("["))
    ("}" '("{"))
    ("`" '("`"))))

(defun jcs-get-close-pair-char (c)
  "Get the list of close pairing character from C."
  (pcase c
    ("\"" '("\""))
    ("'" '("'"))
    ("(" '(")"))
    ("[" '("]"))
    ("{" '("}"))
    ("`" '("`" "'"))))

(defun jcs-forward-delete-close-pair-char (cpc)
  "Forward delete close pair characters CPC."
  (when (and cpc (not (eobp)))
    (save-excursion
      (forward-char 1)
      (when (jcs-current-char-equal-p cpc)
        (backward-delete-char 1)))))

(defun jcs-backward-delete-open-pair-char (opc)
  "Backward delete open pair characters OPC."
  (when (and opc (not (bobp)))
    (save-excursion
      (when (jcs-current-char-equal-p opc)
        (backward-delete-char 1)))))

(defun jcs-forward-delete-close-pair-char-seq (cc)
  "Forward delete close pair characters in sequence.
CC : Current character at position."
  (save-excursion
    (cond ((string= cc "*")  ; Seq => /**/
           (when (jcs-current-char-equal-p "/")
             (save-excursion
               (forward-char 1)
               (when (jcs-current-char-equal-p "*")
                 (forward-char 1)
                 (when (jcs-current-char-equal-p "/")
                   ;; Found sequence, delete them!
                   (backward-delete-char 3)))))))))

(defun jcs-backward-delete-open-pair-char-seq (cc)
  "Backward delete open pair characters in sequence.
CC : Current character at position."
  (save-excursion
    (pcase cc
      ("*"  ; Seq => /**/
       (backward-char 1)
       (when (jcs-current-char-equal-p "/")
         (forward-char 1)
         (when (jcs-current-char-equal-p "*")
           (forward-char 1)
           (when (jcs-current-char-equal-p "/")
             ;; Found sequence, delete them!
             (backward-delete-char 3))))))))

(defun jcs-electric-delete ()
  "Electric delete key."
  (interactive)
  (if (use-region-p) (jcs-delete-region)
    (let ((cc "") (opc ""))
      (save-excursion
        (forward-char 1)
        (setq cc (jcs-get-current-char-string)))
      (setq opc (jcs-get-open-pair-char cc))
      (if (and (jcs-inside-string-p)
               (not (string= cc "\""))
               (not (string= cc "'")))
          (backward-delete-char -1)
        (backward-delete-char -1)
        (jcs-backward-delete-open-pair-char opc)
        (jcs-backward-delete-open-pair-char-seq cc)))))

(defun jcs-own-delete-backward-char ()
  "This isn't the VS like key action, is more likely to be user's own preferences."
  (interactive)
  (save-excursion
    (when (jcs-current-char-equal-p "{")
      (forward-char 1)
      (when (and (not (jcs-beginning-of-line-p))
                 (jcs-current-char-equal-p " "))
        (forward-char 1)
        (when (and (not (jcs-beginning-of-line-p))
                   (jcs-current-char-equal-p "}"))
          (backward-delete-char 1)))))
  (backward-delete-char 1)
  (save-excursion
    (when (jcs-current-char-equal-p "{")
      (forward-char 1)
      (when (and (not (jcs-beginning-of-line-p))
                 (jcs-current-char-equal-p " "))
        (forward-char 1)
        (when (and (not (jcs-beginning-of-line-p))
                   (jcs-current-char-equal-p "}"))
          (backward-char 1)
          (backward-delete-char 1))))))

(defun jcs-electric-backspace ()
  "Electric backspace key."
  (interactive)
  (if (use-region-p) (jcs-delete-region)
    (if (and (jcs-inside-string-p)
             (not (jcs-current-char-equal-p '("\"" "'"))))
        (jcs-own-delete-backward-char)
      (let* ((cc (jcs-get-current-char-string)) (cpc (jcs-get-close-pair-char cc)))
        (jcs-own-delete-backward-char)
        (jcs-forward-delete-close-pair-char cpc)
        (jcs-forward-delete-close-pair-char-seq cc)))))

;;
;; (@* "Isearch" )
;;

(defun jcs-isearch-backward-symbol-at-point ()
  "Isearch backward symbol at point."
  (interactive)
  (isearch-forward-symbol-at-point)
  (isearch-repeat-backward))

(defun jcs-isearch-project-backward-symbol-at-point ()
  "Isearch project backward symbol at point."
  (interactive)
  (isearch-project-forward-symbol-at-point))

(defun jcs--use-isearch-project-p ()
  "Return non-nil is using `isearch-project'."
  (advice-member-p 'isearch-project--advice-isearch-repeat-after 'isearch-repeat))

(defun jcs-isearch-repeat-backward ()
  "Isearch backward repeating."
  (interactive)
  (if (not (jcs--use-isearch-project-p))
      (isearch-repeat-backward)
    (message "Exit 'isearch-project' becuase you are trying to use 'isearch'..")
    (jcs-sleep-for)
    (save-mark-and-excursion (isearch-abort))))

(defun jcs-isearch-repeat-forward ()
  "Isearch forward repeating."
  (interactive)
  (if (not (jcs--use-isearch-project-p))
      (isearch-repeat-forward)
    (message "Exit 'isearch-project' because you are trying to use 'isearch'..")
    (jcs-sleep-for)
    (save-mark-and-excursion (isearch-abort))))

(defun jcs-isearch-project-repeat-backward ()
  "Isearch project backward repeating."
  (interactive)
  (if (jcs--use-isearch-project-p)
      (isearch-repeat-backward)
    (message "Exit 'isearch' because you are trying to use 'isearch-project'..")
    (jcs-sleep-for)
    (save-mark-and-excursion (isearch-abort))))

(defun jcs-isearch-project-repeat-forward ()
  "Isearch project forward repeating."
  (interactive)
  (if (jcs--use-isearch-project-p)
      (isearch-repeat-forward)
    (message "Exit 'isearch' because you are trying to use 'isearch-project'..")
    (jcs-sleep-for)
    (save-mark-and-excursion (isearch-abort))))

;;
;; (@* "Multiple Cursors" )
;;

(defun jcs-mc/mark-previous-like-this-line ()
  "Smart marking previous line."
  (interactive)
  (require 'multiple-cursors)
  (let ((before-unmark-cur-cnt (mc/num-cursors))
        (unmark-do (ignore-errors (call-interactively #'mc/unmark-next-like-this))))
    (unless unmark-do
      (unless (> before-unmark-cur-cnt (mc/num-cursors))
        (call-interactively #'mc/mark-previous-like-this)))))

(defun jcs-mc/mark-next-like-this-line ()
  "Smart marking next line."
  (interactive)
  (require 'multiple-cursors)
  (let ((before-unmark-cur-cnt (mc/num-cursors))
        (unmark-do (ignore-errors (call-interactively #'mc/unmark-previous-like-this))))
    (unless unmark-do
      (unless (> before-unmark-cur-cnt (mc/num-cursors))
        (call-interactively #'mc/mark-next-like-this)))))

(defun jcs-mc/maybe-multiple-cursors-mode ()
  "Maybe enable `multiple-cursors-mode' depends on the cursor number."
  (if (> (mc/num-cursors) 1) (multiple-cursors-mode 1) (multiple-cursors-mode 0)))

(defun jcs-mc/to-furthest-cursor-before-point ()
  "Goto the furthest cursor before point."
  (when (mc/furthest-cursor-before-point) (goto-char (overlay-end (mc/furthest-cursor-before-point)))))

(defun jcs-mc/to-furthest-cursor-after-point ()
  "Goto furthest cursor after point."
  (when (mc/furthest-cursor-after-point) (goto-char (overlay-end (mc/furthest-cursor-after-point)))))

(defun jcs-mc/mark-previous-similar-this-line (&optional sdl)
  "Mark previous line similar to this line depends on string distance level (SDL)."
  (interactive)
  (require 'multiple-cursors)
  (unless sdl (setq sdl jcs-mc/string-distance-level))
  (save-excursion
    (let ((cur-line (thing-at-point 'line)) (cur-col (current-column))
          sim-line break)
      (jcs-mc/to-furthest-cursor-before-point)
      (forward-line -1)
      (while (and (not break) (not (= (line-number-at-pos (point)) (line-number-at-pos (point-min)))))
        (setq sim-line (thing-at-point 'line))
        (when (and (< (string-distance sim-line cur-line) sdl)
                   (or (and (not (string= "\n" sim-line)) (not (string= "\n" cur-line)))
                       (and (string= "\n" sim-line) (string= "\n" cur-line))))
          (move-to-column cur-col)
          (mc/create-fake-cursor-at-point)
          (setq break t))
        (forward-line -1))
      (unless break (user-error "[INFO] no previous similar match"))))
  (jcs-mc/maybe-multiple-cursors-mode))

(defun jcs-mc/mark-next-similar-this-line (&optional sdl)
  "Mark next line similar to this line depends on string distance level (SDL)."
  (interactive)
  (require 'multiple-cursors)
  (unless sdl (setq sdl jcs-mc/string-distance-level))
  (save-excursion
    (let ((cur-line (thing-at-point 'line)) (cur-col (current-column))
          sim-line break)
      (jcs-mc/to-furthest-cursor-after-point)
      (forward-line 1)
      (while (and (not break) (not (= (line-number-at-pos (point)) (line-number-at-pos (point-max)))))
        (setq sim-line (thing-at-point 'line))
        (when (and (< (string-distance sim-line cur-line) sdl)
                   (or (and (not (string= "\n" sim-line)) (not (string= "\n" cur-line)))
                       (and (string= "\n" sim-line) (string= "\n" cur-line))))
          (move-to-column cur-col)
          (mc/create-fake-cursor-at-point)
          (setq break t))
        (forward-line 1))
      (unless break (user-error "[INFO] no next similar match"))))
  (jcs-mc/maybe-multiple-cursors-mode))

(defun jcs-mc/inc-string-distance-level ()
  "Increase the string distance level by 1."
  (interactive)
  (setq jcs-mc/string-distance-level (1+ jcs-mc/string-distance-level))
  (message "[INFO] Current string distance: %s" jcs-mc/string-distance-level))

(defun jcs-mc/dec-string-distance-level ()
  "Decrease the string distance level by 1."
  (interactive)
  (setq jcs-mc/string-distance-level (1- jcs-mc/string-distance-level))
  (message "[INFO] Current string distance: %s" jcs-mc/string-distance-level))

;;
;; (@* "Folding / Unfolding" )
;;

(defun jcs-close-all-nodes ()
  "Close all nodes in current file."
  (interactive)
  (ts-fold-close-all))

(defun jcs-open-all-nodes ()
  "Open all nodes in current file."
  (interactive)
  (ts-fold-open-all))

(defun jcs-vs-close-node ()
  "Close node at the end of line, inspired from Visual Studio."
  (save-excursion
    (end-of-line)
    (when (jcs-inside-comment-p) (back-to-indentation))
    (ts-fold-close)))

(defun jcs-vs-open-node ()
  "Open node at the end of line, inspired from Visual Studio."
  (save-excursion
    (end-of-line)
    (when (jcs-inside-comment-p) (back-to-indentation))
    (let ((before-pt (jcs-point-at-pos (beginning-of-visual-line)))
          after-pt)
      (ts-fold-open)
      (setq after-pt (jcs-point-at-pos (beginning-of-visual-line)))
      (unless (= after-pt before-pt)
        (goto-char before-pt)
        (end-of-line)))))

(defun jcs-close-node ()
  "Close the current scope of the node."
  (interactive)
  (or (jcs-vs-close-node) (ts-fold-close)))

(defun jcs-open-node ()
  "Open the current scope of the node."
  (interactive)
  (or (jcs-vs-open-node) (ts-fold-open)))

(provide 'jcs-edit)
;;; jcs-edit.el ends here
