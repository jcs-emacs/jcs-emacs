;;; jcs-buffer-menu.el --- Functions in buffer menu mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'buffer-menu-project)

;;
;; (@* "Customization" )
;;

(defconst jcs-buffer-menu--default-project-value ""
  "Default value for project column.")

(defun jcs-buffer-menu--name-width (&optional buffer-list)
  "Return max buffer name width by BUFFER-LIST."
  (jcs-buffer-menu--header-width
   "Buffer " (if diminish-buffer-mode (diminish-buffer-diminished-list)
               (diminish-buffer-default-list))
   2))

(defun jcs-buffer-menu--project-width ()
  "Return max project width."
  (require 'f)
  (max (length jcs-buffer-menu--default-project-value)
       (jcs-buffer-menu--header-width "Project " (f-uniquify (jcs-project-opened-projects)))))

(defun jcs-buffer-menu--size-width (buffer-list)
  "Return max buffer size width by BUFFER-LIST."
  (jcs-buffer-menu--header-width
   "Size " (let (sizes)
             (dolist ( buf (or buffer-list (buffer-list)))
               (push (number-to-string (buffer-size buf)) sizes))
             sizes)))

(defun jcs-buffer-menu--header-width (name lst &optional extra)
  "Return the width by NAME and LST."
  (let ((extra (or extra 0)) (min-size (length name)))
    (+ (max min-size (or (jcs-list-max lst) min-size)) extra)))

(defun jcs-buffer-menu--show-project-p ()
  "Return non-nil if we want to show `project' column."
  (and (jcs-project-opened-projects) (not buffer-menu-project-buffers)))

(defun jcs--list-buffers--refresh (&optional buffer-list old-buffer &rest _)
  "Override function `list-buffers--refresh'."
  (let ((name-width (jcs-buffer-menu--name-width buffer-list))
        (size-width (jcs-buffer-menu--size-width buffer-list))
        (marked-buffers (Buffer-menu-marked-buffers))
        (buffer-menu-buffer (current-buffer))
        (show-non-file (not Buffer-menu-files-only))
        (show-project-p (jcs-buffer-menu--show-project-p))
        entries)
    ;; Handle obsolete variable:
    (if Buffer-menu-buffer+size-width
        (setq name-width (- Buffer-menu-buffer+size-width size-width)))
    (setq tabulated-list-format
          (vector '("C" 1 t :pad-right 0)
                  '("R" 1 t :pad-right 0)
                  '("M" 1 t)
                  `("Buffer" ,name-width t)
                  (when show-project-p
                    `("Project" ,(jcs-buffer-menu--project-width) t))
                  `("Size" ,size-width tabulated-list-entry-size-> :right-align t)
                  `("Mode" ,Buffer-menu-mode-width t)
                  '("File" 1 t))
          tabulated-list-format (cl-remove-if #'null tabulated-list-format))
    (setq tabulated-list-use-header-line Buffer-menu-use-header-line)
    ;; Collect info for each buffer we're interested in.
    (dolist (buffer (or buffer-menu-project-buffers
                        buffer-list
                        (buffer-list (if Buffer-menu-use-frame-buffer-list
                                         (selected-frame)))))
      (with-current-buffer buffer
        (let* ((name (buffer-name))
               (file buffer-file-name))
          (when (and (buffer-live-p buffer)
                     (or buffer-list
                         (and (or (not (string= (substring name 0 1) " "))
                                  file)
                              (not (eq buffer buffer-menu-buffer))
                              (or file show-non-file))))
            (push (list buffer
                        (cl-remove-if
                         #'null
                         (vector (cond
                                  ((eq buffer old-buffer) ".")
                                  ((member buffer marked-buffers) ">")
                                  (t " "))
                                 (if buffer-read-only "%" " ")
                                 (if (buffer-modified-p) "*" " ")
                                 (Buffer-menu--pretty-name name)
                                 (if show-project-p
                                     (or (jcs-project-current-uniquify) jcs-buffer-menu--default-project-value)
                                   " ")
                                 (number-to-string (buffer-size))
                                 (concat (format-mode-line mode-name
                                                           nil nil buffer)
                                         (if mode-line-process
                                             (format-mode-line mode-line-process
                                                               nil nil buffer)))
                                 (Buffer-menu--pretty-file-name file))))
                  entries)))))
    (setq tabulated-list-entries (nreverse entries)))
  (tabulated-list-init-header))

(advice-add 'list-buffers--refresh :override #'jcs--list-buffers--refresh)

;;
;; (@* "After load" )
;;

(buffer-menu-filter-mode 1)
(diminish-buffer-mode 1)

(provide 'jcs-buffer-menu)
;;; jcs-buffer-menu.el ends here
