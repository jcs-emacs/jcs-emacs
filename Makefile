SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

.PHONY: startup

# Turn off `debug-on-error` due to error `Failed to download ‘melpa’ archive.`
ENTRY="(progn															 \
																		 \
(require 'url-vars)														 \
																		 \
(let ((debug-on-error nil)												 \
	  (url-show-status nil)												 \
	  (user-emacs-directory default-directory)							 \
	  (user-init-file (expand-file-name \"~/.emacs.d/bin/build.el\"))				 \
	  (load-path (delq default-directory load-path)))					 \
  (load-file user-init-file)											 \
  (run-hooks (quote after-init-hook))									 \
  (run-hooks (quote emacs-startup-hook)))								 \
  (jcs-emacs-version))"

startup:
	@$(EMACS) -nw --batch --eval $(ENTRY)
