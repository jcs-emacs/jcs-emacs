SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

.PHONY: startup

# Turn off `debug-on-error` due to error `Failed to download 'melpa' archive.`
ENTRY="(progn															\
																		\
(require 'url-vars)														\
																		\
(let ((debug-on-error nil)												\
	  (url-show-status nil)												\
	  (user-emacs-directory default-directory)							\
	  (early-init-file (expand-file-name \"~/.emacs.d/early-init.el\")) \
	  (user-init-file (expand-file-name \"~/.emacs.d/init.el\")))		\
	  (load-path (delq default-directory load-path)))					\
)"

startup:
	@$(EMACS) -nw --batch --eval $(ENTRY)
