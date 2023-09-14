;;; lang/ai/config.el  -*- lexical-binding: t; -*-

(use-package openai
  :init
  (setq openai-key (jcs-auth-source-get "api.openai.com")))

(use-package chatgpt
  :hook (chatgpt-mode . sideline-mode))
