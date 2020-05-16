;;; jcs-media.el --- Process media, like audio/video.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ffmpeg-player)
(require 'show-eol)
(require 'jcs-shell)

(defun jcs-media--open-media-window ()
  "Open the media window."
  (when ffmpeg-player--buffer
    (when (window-full-height-p)
      (jcs-balance-split-window-vertically)
      (buf-move-down))
    (jcs-move-to-upmost-window t)
    (switch-to-buffer ffmpeg-player--buffer)
    (jcs-safe-jump-shown-to-buffer "[*]ffmpeg-player[*]: " )
    (shrink-window jcs-windows--enlarge-shrink-times)))

(defun jcs-media-close-media-window ()
  "Close the media window."
  (interactive)
  (jcs-safe-jump-shown-to-buffer
   "[*]ffmpeg-player[*]: "
   (lambda ()
     (let ((bot-window nil))
       (save-selected-window
         (when (ignore-errors (windmove-down)) (setq bot-window (selected-window))))
       (when (jcs-maybe-kill-this-buffer)
         (when (and (not (window-full-height-p))
                    (not (jcs-window-buffer-on-column-p (multi-shell--prefix-name))))
           (jcs-delete-window-downwind)))
       (when bot-window (select-window bot-window)))
     (balance-windows)
     (ffmpeg-player--clean-up))))

(provide 'jcs-media)
;;; jcs-media.el ends here
