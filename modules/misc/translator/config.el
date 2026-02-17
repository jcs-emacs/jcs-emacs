;;; misc/translator/config.el  -*- lexical-binding: t; -*-

(use-package google-translate
  :init
  (setq google-translate-default-source-language "auto"
        google-translate-default-target-language "zh-TW")
  :config
  (jcs-advice-add 'google-translate--search-tkk :override (list 430675 2721866130)))

(use-package gt
  :init
  (setq gt-langs '("auto" "zh-TW")
        gt-default-translator (gt-translator :engines (gt-google-engine)
                                             :render  (gt-buffer-render))))
