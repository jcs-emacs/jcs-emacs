;;; jcs-media.el --- Process media, like audio/video.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'ffmpeg-player)


(defun jcs-media--open-media-window ()
  "Open the media window."
  (when ffmpeg-player--buffer
    (when (window-full-height-p) (jcs-balance-split-window-vertically))
    (buf-move-down)))

(defun jcs-media-close-media-window ()
  "Close the media window."
  (interactive)
  (save-selected-window
    (when (ignore-errors (jcs-jump-shown-to-buffer "[*]ffmpeg-player[*]: "))
      (let ((killed (jcs-maybe-kill-this-buffer)))
        (when killed
          (jcs-delete-window-downwind))))))


(provide 'jcs-media)
;;; jcs-media.el ends here
