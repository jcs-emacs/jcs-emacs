;; This is the start of jcs-plugin.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-plugin.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2017-05-29 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-plugin is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-plugin is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;================================================
;; JayCeS Environment Settings.
;;================================================


;;================================================
;; Find file in project plugin
;; SOURCE(jenchieh): https://github.com/technomancy/find-file-in-project
;;================================================
(load-file "~/.emacs.d/elisp/find-file-in-project.el")

(autoload 'find-file-in-project "find-file-in-project" nil t)
(autoload 'find-file-in-project-by-selected "find-file-in-project" nil t)
(autoload 'find-directory-in-project-by-selected "find-file-in-project" nil t)
(autoload 'ffip-show-diff "find-file-in-project" nil t)
(autoload 'ffip-save-ivy-last "find-file-in-project" nil t)
(autoload 'ffip-ivy-resume "find-file-in-project" nil t)

;; You prefer ido-mode?
;;(setq ffip-prefer-ido-mode t)


;;===============
;; Sublimity
;;-------------
;; URL(jenchieh): https://github.com/zk-phi/sublimity
(require 'sublimity)
(require 'sublimity-scroll)

;; default on or off?
;; NOTE(jenchieh): This also trigger the animate scrolling too.
(sublimity-mode 1)

;; Scroll Speed.
(setq sublimity-scroll-weight 2  ;; [Default : 2]
      sublimity-scroll-drift-length 2)  ;; [Default : 2]

(require 'sublimity-map) ;; experimental
(require 'sublimity-attractive)

(setq sublimity-map-size 0)  ;; [Default : 10]
(setq sublimity-map-fraction 0.3)  ;; [Default : 0.3]
(setq sublimity-map-text-scale -7)  ;; [Default: -7]

;; NOTE(jenchieh): When a positive integer is set, buffer
;; width is truncated to this value and drawn centered. To
;; cancel this feature, set this value nil.
(setq sublimity-attractive-centering-width nil)  ;; [Default : 110]

;; NOTE(jenchieh): With the setting above, minimap is displayed
;; after 5 seconds of idle time. When sublimity-map-set-delay
;; is called with nil, then minimap is shown with no delay. This
;; defers from setting delay to 0, especially when used with
;; sublimity-scroll, in the sense that minimap looks not deleted
;; at all but gets worse performance.

;; ATTENTION(jenchieh): Set it to very hight so it will never
;; reach the timer error.
(sublimity-map-set-delay 40000000)

;; NOTE(jenchieh): sublimity-map-setup-hook will run when
;; minimap is created.
(add-hook 'sublimity-map-setup-hook
          (lambda ()
            (setq buffer-face-mode-face '(:family "Monospace"))
            (buffer-face-mode)))

;; NOTE(jenchieh): Following functions are available to hide
;; some UI parts.
;;(sublimity-attractive-hide-bars)
;;(sublimity-attractive-hide-vertical-border)
;;(sublimity-attractive-hide-fringes)
;;(sublimity-attractive-hide-modelines)

;;; Uniquify
;; NOTE: meaningful names for buffers with the same name from
;; prelude.
;; SOURCE(jenchieh): http://pragmaticemacs.com/emacs/uniquify-your-buffer-names/
;; URL: https://github.com/bbatsov/prelude
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


;;; Preprocessor/Marcos highlight.
(require 'preproc-font-lock)
(preproc-font-lock-global-mode t)
(preproc-font-lock-mode t)
(set-face-attribute 'preproc-font-lock-preprocessor-background
                    nil
                    :background "#333333"
                    :inherit nil)


;;; Blank Mode
(require 'blank-mode)
(autoload 'blank-mode "blank-mode" "Toggle blank visualization." t)
(autoload 'blank-toggle-options "blank-mode" "Toggle local `blank-mode' options." t)
;; All the face can be find here.
;; URL: https://www.emacswiki.org/emacs/BlankMode
(set-face-attribute 'blank-indentation
                    nil
                    :background "grey20"
                    :foreground "aquamarine3")
(set-face-attribute 'blank-trailing
                    nil
                    :background "grey20"
                    :foreground "red")


;;; Neo Tree
(require 'neotree)
(setq neo-window-position t)              ;; set window to the right
(setq neo-smart-open t)

;;; Execute path from shell
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; wgrep
(require 'wgrep)
(setq wgrep-auto-save-buffer t)

;;; Powerline
(require 'powerline)
(powerline-default-theme)
;;(powerline-center-theme)
;;(powerline-center-evil-theme)
;;(powerline-vim-theme)
;;(powerline-nano-theme)

;; NOTE(jenchieh):
;; The separator to use for the default theme.
;; Valid Values: alternate, arrow, arrow-fade, bar, box, brace,
;; butt, chamfer, contour, curve, rounded, roundstub, wave, zigzag,
;; utf-8.
(setq powerline-default-separator 'wave)

;;; Diminish
;; NOTE(jenchieh): Do not show theses modes in the mode line.
(require 'diminish)
(diminish 'abbrev-mode)
(diminish 'auto-complete-mode)
(diminish 'company-mode)
(diminish 'flycheck-mode)
(diminish 'flymake-mode)
(diminish 'helm-mode)
(diminish 'helm-gtags-mode)
(diminish 'impatient-mode)
(diminish 'js2-refactor-mode)
(diminish 'js2r)
(diminish 'outline-mode)
(diminish 'skewer-mode)
(diminish 'yasnippet)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-plugin.el file
