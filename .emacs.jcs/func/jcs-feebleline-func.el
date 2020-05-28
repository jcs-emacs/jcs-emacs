;;; jcs-feebleline-func.el --- Feebleline function related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'f)
(require 'ffmpeg-player)
(require 'show-eol)

(defvar jcs-feebleline-show-lsp-info t
  "Show LSP information.")
(defvar jcs-feebleline-show-symbol-read-only t
  "Show read only symbol.")
(defvar jcs-feebleline-show-major-mode t
  "Show major mode.")
(defvar jcs-feebleline-show-project-name t
  "Show project name.")
(defvar jcs-feebleline-show-buffer-name t
  "Show buffer name.")
(defvar jcs-feebleline-show-coding-system-and-line-endings t
  "Show coding system and line endings.")
(defvar jcs-feebleline-show-spc/tab-and-width t
  "Show space/tab and it's width.")
(defvar jcs-feebleline-show-line/column t
  "Show line and column.")
(defvar jcs-feebleline-show-time t
  "Show time.")
(defvar jcs-feebleline-show-vc-info t
  "Show version control information.")

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


(defun jcs--feebleline--lsp-info ()
  "Feebleline LSP information."
  (if jcs-feebleline-show-lsp-info
      (let ((lsp-connected (jcs--lsp-connected-p)))
        (format " %sLSP%s%s%s"
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
      (propertize "¢"
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

(defun jcs--feebleline--project-name ()
  "Feebleline project name."
  (if jcs-feebleline-show-project-name
      (let ((project-root (jcs-project-current)))
        (format "%s %s %s"
                (propertize "{" 'face jcs--feebleline--separator-face)
                (if (and project-root (buffer-file-name))
                    (file-name-nondirectory (directory-file-name project-root))
                  "¥")
                (propertize "}" 'face jcs--feebleline--separator-face)))
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

(defun jcs--feebleline--vc-info ()
  "Version control info."
  (if (and jcs-feebleline-show-vc-info (jcs-project-current))
      (let ((backend (vc-backend (buffer-file-name (current-buffer)))))
        (unless jcs--vc-current-branch-name
          (setq jcs--vc-current-branch-name (magit-get-current-branch)))
        (if (and backend jcs--vc-current-branch-name)
            (format " %s %s%s%s %s"
                    (propertize "{" 'face jcs--feebleline--separator-face)
                    (propertize (symbol-name backend) 'face jcs--feebleline-vc--info-face)
                    (propertize "-" 'face jcs--feebleline--separator-face)
                    (propertize jcs--vc-current-branch-name 'face jcs--feebleline-vc--info-face)
                    (propertize "}" 'face jcs--feebleline--separator-face))
          ""))
    ""))

(defun jcs--feebleline--coding-system-and-line-endings ()
  "Feebleline coding system and line endings."
  (if jcs-feebleline-show-coding-system-and-line-endings
      (format "%s%s %s %s%s"
              (propertize "[" 'face jcs--feebleline--separator-face)
              buffer-file-coding-system
              (propertize ":" 'face jcs--feebleline--separator-face)
              (show-eol-get-eol-mark-by-system)
              (propertize "]" 'face jcs--feebleline--separator-face))
    ""))

(defun jcs--feebleline--spc/tab-and-width ()
  "Feebleline spaces or tabs."
  (if jcs-feebleline-show-spc/tab-and-width
      (format "%s%s %s %s%s"
              (propertize "[" 'face jcs--feebleline--separator-face)
              (jcs-buffer-spaces-to-tabs)
              (propertize ":" 'face jcs--feebleline--separator-face)
              (jcs-get-tab-width-by-mode)
              (propertize "]" 'face jcs--feebleline--separator-face))
    ""))

(defun jcs--feebleline--line/column ()
  "Feebleline show line numbers and column numbers."
  (if jcs-feebleline-show-line/column
      (format "%s%s %s %s%s"
              (propertize "[" 'face jcs--feebleline--separator-face)
              (feebleline-line-number)
              (propertize ":" 'face jcs--feebleline--separator-face)
              (feebleline-column-number)
              (propertize "]" 'face jcs--feebleline--separator-face))
    ""))

(defun jcs--feebleline--time ()
  "Feebleline time."
  (if jcs-feebleline-show-time
      (format "%s%s%s"
              (propertize "[" 'face jcs--feebleline--separator-face)
              (format-time-string "%Y-%m-%d %H:%M:%S")
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
  (format "[%s-%s]"
          (ffmpeg-player--number-to-string-time ffmpeg-player--video-timer)
          (ffmpeg-player--number-to-string-time ffmpeg-player--current-duration)))

(defun jcs--feebleline--pause-mute-volume ()
  "Video Player's volume."
  (format "[%s:%s:%s]"
          (if ffmpeg-player--pause
              (propertize "‼" 'face jcs--feebleline-pause-face)
            (propertize "►" 'face jcs--feebleline-unpause-face))
          (if ffmpeg-player--mute
              (propertize "Ø" 'face jcs--feebleline-mute-face)
            (propertize "Ö" 'face jcs--feebleline-unmute-face))
          ffmpeg-player--volume))

(provide 'jcs-feebleline-func)
;;; jcs-feebleline-func.el ends here
