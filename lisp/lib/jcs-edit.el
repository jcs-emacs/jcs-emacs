;;; jcs-edit.el --- When editing the file  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "JIT lock" )
;;

;; Inhibit error output
(jcs-advice-add 'jit-lock-function :around (ignore-errors (apply arg0 args)))

;;
;; (@* "Move Between Word (Wrapper)" )
;;

(defun jcs-backward-word-capital (&optional _)
  "Backward search capital character and set the cursor to the point."
  (interactive "^P")
  (let ((max-pt (save-excursion (vs-edit-backward-word) (1+ (point)))))
    (while (and (not (bobp))
                (not (jcs-current-char-uppercasep))
                (> (point) max-pt))
      (backward-char 1))
    (backward-char 1)))

(defun jcs-forward-word-capital (&optional _)
  "Forward search capital character and set the cursor to the point."
  (interactive "^P")
  (let ((max-pt (save-excursion (vs-edit-forward-word) (point))))
    (forward-char 1)
    (while (and (not (eobp))
                (not (jcs-current-char-uppercasep))
                (< (point) max-pt))
      (forward-char 1))))

;;
;; (@* "Balanced Expression (sexp)" )
;;

(defun jcs-current-pair ()
  "Return current pair character."
  (let* ((prev (char-before))
         (next (char-after))
         (syntax-info (and prev
                           (electric-pair-syntax-info prev)))
         (syntax (car syntax-info))
         (pair (cadr syntax-info)))
    (ignore-errors (string pair))))

(defun jcs-backward-sexp ()
  "Wrapper for function `backward-sexp'."
  (interactive)
  (cond ((jcs-current-pair) (backward-sexp))
        ((save-excursion (forward-char 1) (jcs-current-pair))
         (forward-char 1)
         (backward-sexp))
        (t (user-error "[INFO] You are at the end of backward sexp"))))

(defun jcs-forward-sexp ()
  "Wrapper for function `forward-sexp'."
  (interactive)
  (cond ((save-excursion (forward-char 1) (jcs-current-pair))
         (forward-sexp))
        ((jcs-current-pair)
         (forward-char -1)
         (forward-sexp))
        (t (user-error "[INFO] You are at the end of forward sexp"))))

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

(jcs-advice-add 'indent-region :around
  (ignore-errors (apply arg0 args)))

(jcs-advice-add 'indent-line-to :before (indent-control-ensure-tab-width))

;;
;; (@* "Return" )
;;

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
;; (@* "Overwrite" )
;;

(jcs-add-hook 'overwrite-mode-hook
  (require 'multiple-cursors)
  (electric-cursor-mode 1)
  (cond (overwrite-mode
         (set-face-attribute 'mc/cursor-face nil :underline t :inverse-video nil))
        (t (set-face-attribute 'mc/cursor-face nil :underline nil :inverse-video t))))

;;
;; (@* "Kill Ring" )
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

;;
;; (@* "Format File" )
;;

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
;; (@* "Save Buffer" )
;;

(jcs-advice-add 'save-buffer :before
  (jcs-funcall-fboundp #'company-abort)
  ;; Delete trailing whitespaces execpt the current line
  (when (bound-and-true-p whitespace-cleanup-mode)
    (whitespace-cleanup-region (point-min) (line-beginning-position))
    (whitespace-cleanup-region (line-end-position) (point-max))))

(defun jcs-save-all-buffers ()
  "Save all buffers currently opened."
  (interactive)
  (let (saved-lst)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when-let ((result
                    (ignore-errors
                      (msgu-silent
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
    (msgu-inhibit-log
      (message "[WARN] Can't save with invalid filename: %s" (buffer-name))))
   (buffer-read-only
    (msgu-inhibit-log
      (message "[WARN] Can't save read-only file: %s" buffer-read-only)))
   (t
    (let ((readable (file-readable-p (buffer-file-name))))
      (msgu-inhibit-log (call-interactively #'save-buffer))
      (unless readable (jcs-lsp-safe-active))))))

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

(jcs-advice-add 'bury-buffer :after
  (run-hooks 'buffer-list-update-hook))

(defun jcs-bury-diminished-buffer ()
  "Bury the diminished buffer."
  (when (and diminish-buffer-mode
             (diminish-buffer--filter (current-buffer)))
    (jcs-bury-buffer)))

(defun jcs-bury-buffer ()
  "Bury this buffer."
  (interactive)
  (let ((bn (jcs-buffer-name-or-buffer-file-name)))
    (jcs-funcall-fboundp #'undo-tree-kill-visualizer)
    (bury-buffer)
    (when (or (jcs-buffer-menu-p)
              (string= bn (jcs-buffer-name-or-buffer-file-name)))
      (bury-buffer)))
  ;; If something that I doesn't want to see, bury it.
  ;; For instance, any `*helm-' buffers.
  (jcs-bury-diminished-buffer))

(defun jcs-kill-this-buffer ()
  "Kill this buffer."
  (interactive)
  (jcs-lsp-maybe-shutdown)
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
  (let ((shown-multiple-p (jcs-buffer-shown-in-multiple-window-p (buffer-name) 'strict))
        (cur-buf (current-buffer))
        is-killed)
    (cond
     ;; (1) Centain conditions, we bury it!
     ((or shown-multiple-p
          (and (jcs-virtual-buffer-p) (not (jcs-invalid-buffer-p))))
      (jcs-bury-buffer))
     ;; (2) Else, we kill it!
     (t
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
          (jcs-switch-to-next-valid-buffer)))))
    ;; If something that I doesn't want to see, bury it.
    ;; For instance, any `*helm-' buffers.
    (jcs-bury-diminished-buffer)
    is-killed))

(defun jcs-reopen-this-buffer ()
  "Kill the current buffer and open it again."
  (interactive)
  (when-let ((buffer (buffer-file-name)))
    (msgu-inhibit-log
      (when (jcs-lsp-connected-p) (lsp-disconnect))
      (jcs-save-window-excursion (jcs-kill-this-buffer))
      (jcs-funcall-fboundp #'undo-tree-kill-visualizer)
      (msgu-current "[INFO] Reopened file => '%s'" buffer))))

;;
;; (@* "Isearch" )
;;

(defun jcs-isearch-backward-thing-at-point ()
  "Isearch backward thing at point."
  (interactive)
  (isearch-forward-thing-at-point)
  (isearch-repeat-backward))

(defun jcs-isearch-project-backward-thing-at-point ()
  "Isearch project backward symbol at point."
  (interactive)
  (isearch-project-forward-thing-at-point))

(defun jcs--use-isearch-project-p ()
  "Return non-nil is using `isearch-project'."
  (advice-member-p 'isearch-project--advice-isearch-repeat-after 'isearch-repeat))

(defun jcs-isearch-repeat-backward ()
  "Isearch backward repeating."
  (interactive)
  (if (not (jcs--use-isearch-project-p))
      (isearch-repeat-backward)
    (message "Exit 'isearch-project' becuase you are trying to use 'isearch'..")
    (msgu-sleep)
    (save-mark-and-excursion (isearch-abort))))

(defun jcs-isearch-repeat-forward ()
  "Isearch forward repeating."
  (interactive)
  (if (not (jcs--use-isearch-project-p))
      (isearch-repeat-forward)
    (message "Exit 'isearch-project' because you are trying to use 'isearch'..")
    (msgu-sleep)
    (save-mark-and-excursion (isearch-abort))))

(defun jcs-isearch-project-repeat-backward ()
  "Isearch project backward repeating."
  (interactive)
  (if (jcs--use-isearch-project-p)
      (isearch-repeat-backward)
    (message "Exit 'isearch' because you are trying to use 'isearch-project'..")
    (msgu-sleep)
    (save-mark-and-excursion (isearch-abort))))

(defun jcs-isearch-project-repeat-forward ()
  "Isearch project forward repeating."
  (interactive)
  (if (jcs--use-isearch-project-p)
      (isearch-repeat-forward)
    (message "Exit 'isearch' because you are trying to use 'isearch-project'..")
    (msgu-sleep)
    (save-mark-and-excursion (isearch-abort))))

(provide 'jcs-edit)
;;; jcs-edit.el ends here
