;; ========================================================================
;; $File: jcs-after-init.el $
;; $Date: 2017-08-04 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Do stuff after initialize.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


(mapc (lambda (mode)
        (let ((case-fold-search t))
          (font-lock-add-keywords
           mode
           '(;; NOTE(jenchieh): Coding Use keywords.
             ("\\<\\(TODO\\)\\>" 1 'jcs-font-lock-fixme-face t)
             ("\\<\\(ATTENTION\\)\\>" 1 'jcs-font-lock-attention-face t)
             ("\\<\\(STUDY\\)\\>" 1 'jcs-font-lock-study-face t)
             ("\\<\\(CAUTION\\)\\>" 1 'jcs-font-lock-caution-face t)
             ("\\<\\(IMPORTANT\\)\\>" 1 'jcs-font-lock-important-face t)
             ("\\<\\(OPTIMIZE\\)\\>" 1 'jcs-font-lock-optimize-face t)
             ("\\<\\(NOTE\\)\\>" 1 'jcs-font-lock-note-face t)
             ("\\<\\(DESC\\)\\>" 1 'jcs-font-lock-description-face t)
             ("\\<\\(DESCRIPTION\\)\\>" 1 'jcs-font-lock-description-face t)
             ("\\<\\(TAG\\)\\>" 1 'jcs-font-lock-tag-face t)
             ("\\<\\(DEBUG\\)\\>" 1 'jcs-font-lock-debugging-face t)
             ("\\<\\(DEBUGGING\\)\\>" 1 'jcs-font-lock-debugging-face t)
             ("\\<\\(TEMP\\)\\>" 1 'jcs-font-lock-temporary-face t)
             ("\\<\\(TEMPORARY\\)\\>" 1 'jcs-font-lock-temporary-face t)
             ("\\<\\(SOURCE\\)\\>" 1 'jcs-font-lock-source-face t)
             ("\\<\\(URL\\)\\>" 1 'jcs-font-lock-url-face t)
             ("\\<\\(IDEA\\)\\>" 1 'jcs-font-lock-idea-face t)
             ("\\<\\(OBSOLETE\\)\\>" 1 'jcs-font-lock-obsolete-face t)
             ("\\<\\(DEPRECATED\\)\\>" 1 'jcs-font-lock-deprecated-face t)
             ("\\<\\(TOPIC\\)\\>" 1 'jcs-font-lock-topic-face t)
             ("\\<\\(SEE\\)\\>" 1 'jcs-font-lock-see-face t)

             ;; NOTE(jenchieh): Alternative keywords.
             ("\\<\\(OPTION\\)\\>" 1 'jcs-font-lock-option-face t)
             ("\\<\\(OR\\)\\>" 1 'jcs-font-lock-or-face t)

             ;; NOTE(jenchieh): Special keywords.
             ("`\\([a-zA-Z0-9_$-.!]*\\)'" 1 'jcs-font-lock-key-highlight-face t)
             )'end)))
      jcs-fixme-modes)


;;; Override all the mode's key bindings.
(load-file "~/.emacs.jcs/jcs-global-key.el")


;; Call once the depends mode as default.
;; NOTE(jayces): Since I often use my own machine, set the online
;; mode as the default.
(call-interactively 'jcs-depend-mode)


;;; Diminish
;; NOTE(jenchieh): Do not show theses modes in the mode line.
(require 'diminish)
(diminish 'abbrev-mode)
(diminish 'auto-complete-mode)
(diminish 'auto-highlight-symbol-mode)
(diminish 'auto-rename-tag-mode)
(diminish 'undo-tree-mode)
(diminish 'company-mode)
(diminish 'flycheck-mode)
(diminish 'flymake-mode)
(diminish 'helm-mode)
(diminish 'helm-gtags-mode)
(diminish 'impatient-mode)
(diminish 'js2-refactor-mode)
(diminish 'js2r)
(diminish 'line-reminder-mode)
(diminish 'outline-minor-mode)
(diminish 'right-click-context-mode)
(diminish 'skewer-mode)
(diminish 'which-key-mode)
(diminish 'yas-minor-mode)
