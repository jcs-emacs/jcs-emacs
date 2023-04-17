;;; lang/ai/config.el  -*- lexical-binding: t; -*-

(use-package openai
  :init
  (setq openai-key (jcs-auth-source-get "api.openai.com")))
