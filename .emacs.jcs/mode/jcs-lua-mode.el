;; ========================================================================
;; $File: jcs-lua-mode.el $
;; $Date: 2017-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================

;; NOTE(jenchieh): Load it once!
(font-lock-add-keywords
 'lua-mode
 '(;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   ;; NOTE(jenchieh): Fixed comment and string conflict.
   ("[^\"]\\(--[^\"\r\n]*\\)[^\"]" 1 'jcs-font-lock-comment-face t)
   ("[^\"]\\(\"[^\"]*\"\\)[^\"]" 1 'jcs-font-lock-string-face t)
   ;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
   )'end)


(require 'lua-mode)
(defun jcs-lua-mode-hook ()
  "Lau mode hook."

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")


  (defun jcs-lua-script-format ()
    "Format the given file as a Lua script."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-lua-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]lua" buffer-file-name) (jcs-lua-script-format))
          ((string-match "[.]luac" buffer-file-name) (jcs-lua-script-format))
          ))

  ;; jcs Lua key binding
  (define-key lua-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key lua-mode-map "\C-c\C-c" #'kill-ring-save)

  ;; Comment
  (define-key lua-mode-map (kbd "-") #'jcs-lua-maybe-insert-codedoc)

  ;; comment block
  (define-key lua-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  )
(add-hook 'lua-mode-hook 'jcs-lua-mode-hook)

(add-to-list 'auto-mode-alist '("\\.lua'?\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.luac'?\\'" . lua-mode))
