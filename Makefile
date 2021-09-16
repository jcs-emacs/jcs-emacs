SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

.PHONY: startup

INIT="(let ((debug-on-error (>= emacs-major-version 26)) \
			 (url-show-status nil) \
			 (user-emacs-directory default-directory) \
			 (user-init-file (expand-file-name \"~/build.el\")) \
			 (load-path (delq default-directory load-path))) \
		(run-hooks (quote after-init-hook)) \
		(run-hooks (quote after-init-hook)) \
		(run-hooks (quote emacs-startup-hook)))"

startup:
	@$(EMACS) -Q --batch --eval $(INIT)
