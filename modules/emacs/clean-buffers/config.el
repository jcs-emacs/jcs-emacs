;;; emacs/clean-buffers/config.el  -*- lexical-binding: t; -*-

(use-package clean-buffers
  :init
  (setq clean-buffers-kill-active-buffer t
        clean-buffers-useless-buffer-names
        '("\\*CPU-Profiler-Report "
          "\\*Memory-Profiler-Report "
          "\\*esup")
        clean-buffers-useful-buffer-names nil))
