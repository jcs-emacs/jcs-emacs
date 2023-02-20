;;; misc/translator/config.el  -*- lexical-binding: t; -*-

(use-package google-translate
  :init
  (setq google-translate-default-source-language "auto"
        google-translate-default-target-language "zh-TW")
  :config
  (jcs-advice-add 'google-translate--search-tkk :override (list 430675 2721866130)))
