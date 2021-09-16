SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

.PHONY: startup unix-copy-config windows-copy-config

INIT="(let ((debug-on-error (>= emacs-major-version 26)) \
			 (url-show-status nil) \
			 (user-emacs-directory default-directory) \
			 (user-init-file (expand-file-name \"~/build.el\")) \
			 (load-path (delq default-directory load-path))) \
		(run-hooks (quote after-init-hook)) \
		(run-hooks (quote after-init-hook)) \
		(run-hooks (quote emacs-startup-hook)))"

unix-copy-config:
	sh ./scripts/copy-config.sh

windows-copy-config:
	./scripts/copy-config.bat

startup:
	@$(EMACS) -Q --batch --eval $(INIT)
