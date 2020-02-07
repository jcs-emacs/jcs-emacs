;;; jcs-feebleline-func.el --- Feebleline function related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'f)
(require 'ffmpeg-player)
(require 'show-eol)


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

(defface jcs--feebleline-lsp--expr-face
  '((t (:foreground "#858585")))
  "Face for LSP expression symbol."
  :group 'jcs)
(defvar jcs--feebleline-lsp--expr-face 'jcs--feebleline-lsp--expr-face)


(defun jcs--feebleline--lsp-info ()
  "Feebleline LSP information."
  (if jcs-feebleline-show-lsp-info
      (let ((lsp-managed-mode (if (boundp 'lsp-managed-mode) lsp-managed-mode nil)))
        (format " [LSP%s%s]"
                (propertize "::" 'face jcs--feebleline-lsp--expr-face)
                (propertize (if lsp-managed-mode
                                "connect"
                              "disconnect")
                            'face (if lsp-managed-mode
                                      jcs--feebleline-lsp--connect-face
                                    jcs--feebleline-lsp--disconnect-face))))
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
      (format "[%s]" (jcs-current-major-mode))
    ""))

(defun jcs--feebleline--project-name ()
  "Feebleline project name."
  (if jcs-feebleline-show-project-name
      (let ((project-root (cdr (project-current))))
        (format "{ %s }"
                (if (and project-root (buffer-file-name))
                    (file-name-nondirectory (directory-file-name project-root))
                  "¥")))
    ""))

(defun jcs--feebleline--buffer-name ()
  "Feebleline buffer name."
  (if jcs-feebleline-show-buffer-name
      (format "%s%s" (if (feebleline-file-modified-star)
                         (format "%s " (feebleline-file-modified-star))
                       "")
              (f-filename (jcs-buffer-name-or-buffer-file-name)))
    ""))

(defun jcs--feebleline--coding-system-and-line-endings ()
  "Feebleline coding system and line endings."
  (if jcs-feebleline-show-coding-system-and-line-endings
      (format "[%s : %s]" buffer-file-coding-system (show-eol-get-eol-mark-by-system))
    ""))

(defun jcs--feebleline--spc/tab-and-width ()
  "Feebleline spaces or tabs."
  (if jcs-feebleline-show-spc/tab-and-width
      (format "[%s : %s]" (jcs-buffer-spaces-to-tabs) (jcs-get-tab-width-by-mode))
    ""))

(defun jcs--feebleline--line/column ()
  "Feebleline show line numbers and column numbers."
  (if jcs-feebleline-show-line/column
      (format "[%s : %s]" (feebleline-line-number) (feebleline-column-number))
    ""))

(defun jcs--feebleline--time ()
  "Feebleline time."
  (if jcs-feebleline-show-time
      (format-time-string "[%Y-%m-%d %H:%M:%S]")
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
