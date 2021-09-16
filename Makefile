SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

.PHONY: startup

INIT="(progn															 \
																		 \
(require 'url-vars)														 \
																		 \
(let ((debug-on-error nil)												 \
	  (url-show-status nil)												 \
	  (user-emacs-directory default-directory)							 \
	  (user-init-file (expand-file-name \"~/build.el\"))				 \
	  (load-path (delq default-directory load-path)))					 \
  (load-file user-init-file)											 \
  (run-hooks (quote after-init-hook))									 \
  (run-hooks (quote emacs-startup-hook)))								 \
  (jcs-emacs-version))"

startup:
	@$(EMACS) -nw --batch --eval $(INIT)
