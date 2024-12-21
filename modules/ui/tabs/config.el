;;; ui/tabs/config.el  -*- lexical-binding: t; -*-

(use-package centaur-tabs
  :init
  (setq centaur-tabs-style "wave"
        centaur-tabs-set-bar 'under
        centaur-tabs-set-icons t
        centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-icons-prefix ""
        centaur-tabs-icon-scale-factor 0.9
        centaur-tabs-set-modified-marker t
        centaur-tabs-enable-ido-completion nil
        centaur-tabs-buffer-groups-function #'jcs-tabs-buffer-groups
        centaur-tabs-custom-buffer-groups #'jcs-tabs-custom-buffer-groups
        centaur-tabs-hide-predicate #'elenv-frame-util-p
        centaur-tabs-hide-tab-function #'centaur-tabs-hide-tab
        centaur-tabs-excluded-prefixes `(" *which")
        centaur-tabs-show-count t
        centaur-tabs-count-format " %d/%d"
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-down-tab-text " ▾ "
        centaur-tabs-backward-tab-text "⏴"
        centaur-tabs-forward-tab-text "⏵"
        ;; Scrolling (with the mouse wheel) past the end of the tab list
        ;; replaces the tab list with that of another Doom workspace. This
        ;; prevents that.
        centaur-tabs-cycle-scope 'tabs)
  :config
  (advice-add 'centaur-tabs-buffer-track-killed :override #'ignore))

;;
;;; Buffer Groups

(defvar jcs-tabs-line--group-cache (make-hash-table :test #'equal)
  "Cache for buffer groups.")

(defun jcs-tabs-clear-dead-buffers ()
  "Remove all dead buffers from group cache."
  (ht-map (lambda (buffer _)
            (unless (buffer-live-p buffer)
              (ht-remove jcs-tabs-line--group-cache buffer)))
          jcs-tabs-line--group-cache))

(defun jcs-tabs-buffer-groups ()
  "Group tabs with cache."
  (let* ((name (buffer-name))
         (buffer (current-buffer))
         (group (or (ht-get jcs-tabs-line--group-cache buffer)
                    (car (funcall #'centaur-tabs-buffer-groups)))))
    (jcs-tabs-clear-dead-buffers)
    (ht-set jcs-tabs-line--group-cache buffer group)
    `(,group)))

(defun jcs-tabs-custom-buffer-groups ()
  "Group tabs."
  (let ((name (buffer-name)))
    (cond ((cl-some (lambda (item) (equal item name))
                    '("*snow*"))
           "Screen Saver")
          ((string-prefix-p "*cider" name) "Cider")
          ((string-prefix-p "*sly" name) "Sly")
          ((string-prefix-p "*esup" name) "ESUP")
          ((string-match-p "lsp" name) "LSP-mode")
          ((and (featurep 'buffer-menu-filter)
                (diminish-buffer--filter name))
           "Hidden"))))
