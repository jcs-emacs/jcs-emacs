;;; jcs-media.el --- Process media, like audio/video.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'ffmpeg-player)


(defun jcs-media--open-media-window ()
  "Open the media window."
  (when (not ffmpeg-player--buffer)
    ))

(defun jcs-media--close-media-window ()
  "Close the media window."
  (when (ignore-errors (jcs-jump-shown-to-buffer "*ffmpeg-player*: "))
    (jcs-delete-window-downwind)
    ))


(provide 'jcs-media)
;;; jcs-media.el ends here
