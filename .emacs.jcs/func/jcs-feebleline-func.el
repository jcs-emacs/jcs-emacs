;;; jcs-feebleline-func.el --- Feebleline function related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'f)
(require 'ffmpeg-player)
(require 'show-eol)

(defconst jcs-feebleline--read-only-symbol (if (display-graphic-p) "¢" "&")
  "Symbol display for read-only status.")

(defvar jcs-feebleline-show-lsp-info t
  "Show LSP information.")
(defvar jcs-feebleline-show-symbol-read-only t
  "Show read only symbol.")
(defvar jcs-feebleline-show-major-mode t
  "Show major mode.")
(defvar jcs-feebleline-show-project-name-&-vc-info t
  "Show project name and version control information.")
(defvar jcs-feebleline-show-buffer-name t
  "Show buffer name.")
(defvar jcs-feebleline-show-coding-system-&-line-endings t
  "Show coding system and line endings.")
(defvar jcs-feebleline-show-spc/tab-&-width t
  "Show space/tab and it's width.")
(defvar jcs-feebleline-show-line/column t
  "Show line and column.")
(defvar jcs-feebleline-show-time t
  "Show time.")

;; TODO: When project name changes, update this variable!
(defvar-local jcs--project-name nil "Record down the project name.")
;; TODO: When version control changes, update this variable!
(defvar-local jcs--vc-current-vc-name nil "Record down the current VC name.")
;; TODO: When branch changes, update this variable!
(defvar-local jcs--vc-current-branch-name nil "Record down the branch name.")

(defface jcs--feebleline--separator-face
  '((t (:foreground "#858585")))
  "Face for separator.."
  :group 'jcs)
(defvar jcs--feebleline--separator-face 'jcs--feebleline--separator-face)

(defface jcs--feebleline-read-only--enabled-face
  '((t (:foreground "#FF0000")))
  "Ready only symbol face when active.."
  :group 'jcs)
(defvar jcs--feebleline-read-only--enabled-face 'jcs--feebleline-read-only--enabled-face)

(defface jcs--feebleline-read-only--disabled-face
  '((t (:foreground "#00FF00")))
  "Ready only symbol face when deactive."
  :group 'jcs)
(defvar jcs--feebleline-read-only--disabled-face 'jcs--feebleline-read-only--disabled-face)

(defface jcs--feebleline-lsp--connect-face
  '((t (:foreground "#6DDE6D")))
  "Face when LSP is active."
  :group 'jcs)
(defvar jcs--feebleline-lsp--connect-face 'jcs--feebleline-lsp--connect-face)

(defface jcs--feebleline-lsp--disconnect-face
  '((t (:foreground "#FF7575")))
  "Face when LSP is inactive."
  :group 'jcs)
(defvar jcs--feebleline-lsp--disconnect-face 'jcs--feebleline-lsp--disconnect-face)

(defface jcs--feebleline-vc--info-face
  '((t (:foreground "#F26D55")))
  "Face for Version Control information."
  :group 'jcs)
(defvar jcs--feebleline-vc--info-face 'jcs--feebleline-vc--info-face)


(defun jcs--feebleline--prepare ()
  "Initialize variables that use for `feebleline'."
  (unless jcs--project-name (setq jcs--project-name (jcs-project-current)))
  (when jcs--project-name
    (unless jcs--vc-current-vc-name
      (setq jcs--vc-current-vc-name (vc-responsible-backend (jcs-buffer-name-or-buffer-file-name))))
    (unless jcs--vc-current-branch-name
      (setq jcs--vc-current-branch-name (magit-get-current-branch))))
  "")

(defun jcs--feebleline--lsp-info ()
  "Feebleline LSP information."
  (if jcs-feebleline-show-lsp-info
      (let ((lsp-connected (jcs--lsp-connected-p)))
        (format "%sLSP%s%s%s"
                (propertize "[" 'face jcs--feebleline--separator-face)
                (propertize "::" 'face jcs--feebleline--separator-face)
                (propertize (if lsp-connected "connect" "disconnect")
                            'face (if lsp-connected
                                      jcs--feebleline-lsp--connect-face
                                    jcs--feebleline-lsp--disconnect-face))
                (propertize "]" 'face jcs--feebleline--separator-face)))
    ""))

(defun jcs--feebleline--symbol-read-only ()
  "Feebleline read-only symbol."
  (if jcs-feebleline-show-symbol-read-only
      (propertize jcs-feebleline--read-only-symbol
                  'face (if buffer-read-only
                            jcs--feebleline-read-only--enabled-face
                          jcs--feebleline-read-only--disabled-face))
    ""))

(defun jcs--feebleline--major-mode ()
  "Feebleline major mode."
  (if jcs-feebleline-show-major-mode
      (format "%s%s%s"
              (propertize "[" 'face jcs--feebleline--separator-face)
              (propertize (symbol-name (jcs-current-major-mode)) 'face font-lock-constant-face)
              (propertize "]" 'face jcs--feebleline--separator-face))
    ""))

(defun jcs--feebleline--buffer-name ()
  "Feebleline buffer name."
  (if jcs-feebleline-show-buffer-name
      (let ((str-star (if (feebleline-file-modified-star)
                          (format "%s " (feebleline-file-modified-star))
                        ""))
            (fn (f-filename (jcs-buffer-name-or-buffer-file-name))))
        (format "%s%s"
                (propertize str-star 'face font-lock-keyword-face)
                (propertize fn 'face font-lock-keyword-face)))
    ""))

(defun jcs--feebleline--project-name-&-vc-info ()
  "Feebleline project name and version control information."
  (if jcs-feebleline-show-project-name-&-vc-info
      (let ((valid-project-name-p (and jcs--project-name (buffer-file-name)))
            (valid-vc-p (and jcs--vc-current-vc-name jcs--vc-current-branch-name)))
        (if (and valid-project-name-p valid-vc-p)
            (format " %s%s%s %s%s%s%s"
                    ;; Project Name
                    (propertize "<" 'face jcs--feebleline--separator-face)
                    (file-name-nondirectory (directory-file-name jcs--project-name))
                    (propertize "," 'face jcs--feebleline--separator-face)
                    ;; VC info
                    (propertize (symbol-name jcs--vc-current-vc-name) 'face jcs--feebleline-vc--info-face)
                    (propertize "-" 'face jcs--feebleline--separator-face)
                    (propertize jcs--vc-current-branch-name 'face jcs--feebleline-vc--info-face)
                    (propertize ">" 'face jcs--feebleline--separator-face))
          ""))
    ""))

(defun jcs--feebleline--coding-system-&-line-endings ()
  "Feebleline coding system and line endings."
  (if jcs-feebleline-show-coding-system-&-line-endings
      (format "%s%s%s%s%s"
              (propertize "[" 'face jcs--feebleline--separator-face)
              buffer-file-coding-system
              (propertize "::" 'face jcs--feebleline--separator-face)
              (show-eol-get-eol-mark-by-system)
              (propertize "]" 'face jcs--feebleline--separator-face))
    ""))

(defun jcs--feebleline--spc/tab-&-width ()
  "Feebleline spaces or tabs."
  (if jcs-feebleline-show-spc/tab-&-width
      (format "%s%s%s%s%s"
              (propertize "[" 'face jcs--feebleline--separator-face)
              (jcs-buffer-spaces-to-tabs)
              (propertize "::" 'face jcs--feebleline--separator-face)
              (jcs-get-tab-width-by-mode)
              (propertize "]" 'face jcs--feebleline--separator-face))
    ""))

(defun jcs--feebleline--line/column ()
  "Feebleline show line numbers and column numbers."
  (if jcs-feebleline-show-line/column
      (format "%s%s%s%s%s"
              (propertize "[" 'face jcs--feebleline--separator-face)
              (feebleline-line-number)
              (propertize "::" 'face jcs--feebleline--separator-face)
              (feebleline-column-number)
              (propertize "]" 'face jcs--feebleline--separator-face))
    ""))

(defun jcs--feebleline--time ()
  "Feebleline time."
  (if jcs-feebleline-show-time
      (format "%s%s%s"
              (propertize "[" 'face jcs--feebleline--separator-face)
              (format "%s%s%s%s%s %s%s%s%s%s"
                      (format-time-string "%Y")
                      (propertize "-" 'face jcs--feebleline--separator-face)
                      (format-time-string "%m")
                      (propertize "-" 'face jcs--feebleline--separator-face)
                      (format-time-string "%d")
                      (format-time-string "%H")
                      (propertize ":" 'face jcs--feebleline--separator-face)
                      (format-time-string "%M")
                      (propertize ":" 'face jcs--feebleline--separator-face)
                      (format-time-string "%S"))
              (propertize "]" 'face jcs--feebleline--separator-face))
    ""))

;;; Video Player

(defface jcs--feebleline-mute-face
  '((t (:foreground "#FF0000")))
  "Video player mute face."
  :group 'jcs)
(defvar jcs--feebleline-mute-face 'jcs--feebleline-mute-face)

(defface jcs--feebleline-unmute-face
  '((t (:foreground "#00FF00")))
  "Video player unmute face."
  :group 'jcs)
(defvar jcs--feebleline-unmute-face 'jcs--feebleline-unmute-face)

(defface jcs--feebleline-pause-face
  '((t (:foreground "#FF0000")))
  "Video player pause face."
  :group 'jcs)
(defvar jcs--feebleline-pause-face 'jcs--feebleline-pause-face)

(defface jcs--feebleline-unpause-face
  '((t (:foreground "#00FF00")))
  "Video player pause face."
  :group 'jcs)
(defvar jcs--feebleline-unpause-face 'jcs--feebleline-unpause-face)


(defun jcs--feebleline--timeline ()
  "Video Player's timeline."
  (format "%s%s%s%s%s"
          (propertize "[" 'face jcs--feebleline--separator-face)
          (ffmpeg-player--number-to-string-time ffmpeg-player--video-timer)
          (propertize "-" 'face jcs--feebleline--separator-face)
          (ffmpeg-player--number-to-string-time ffmpeg-player--current-duration)
          (propertize "]" 'face jcs--feebleline--separator-face)))

(defun jcs--feebleline--pause-mute-volume ()
  "Video Player's volume."
  (format "%s%s:%s:%s%s"
          (propertize "[" 'face jcs--feebleline--separator-face)
          (if ffmpeg-player--pause
              (propertize "‼" 'face jcs--feebleline-pause-face)
            (propertize "►" 'face jcs--feebleline-unpause-face))
          (if ffmpeg-player--mute
              (propertize "Ø" 'face jcs--feebleline-mute-face)
            (propertize "Ö" 'face jcs--feebleline-unmute-face))
          ffmpeg-player--volume
          (propertize "]" 'face jcs--feebleline--separator-face)))

(provide 'jcs-feebleline-func)
;;; jcs-feebleline-func.el ends here
