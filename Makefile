SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

.PHONY: startup

# Turn off `debug-on-error` due to error `Failed to download 'melpa' archive.`
ENTRY="(progn													   \
																   \
(require 'url-vars)												   \
																   \
(let ((debug-on-error nil)										   \
	  (url-show-status nil)										   \
	  (user-emacs-directory default-directory)					   \
	  (early-init-file (locate-user-emacs-file \"early-init.el\")) \
	  (user-init-file (locate-user-emacs-file \"bin/build.el\")))	   \
  (load early-init-file)										   \
  (load user-init-file)											   \
  (run-hooks after-init-hook)									   \
  (run-hooks emacs-startup-hook))								   \
  (jcs-emacs-version))"

startup:
	@$(EMACS) -nw --batch --eval $(ENTRY)
