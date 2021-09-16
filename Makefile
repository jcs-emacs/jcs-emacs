SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

.PHONY: unix-startup windows-startup

UNIX-ENTRY="(progn															  \
																			  \
(require 'url-vars)															  \
																			  \
(let ((debug-on-error t)													  \
	  (url-show-status nil)													  \
	  (user-emacs-directory default-directory)								  \
	  (user-init-file (expand-file-name \"~/build.el\"))					  \
	  (load-path (delq default-directory load-path)))						  \
  (load-file user-init-file)												  \
  (run-hooks (quote after-init-hook))										  \
  (run-hooks (quote emacs-startup-hook))))"

WINDOWS-ENTRY="(progn														  \
																			  \
(require 'url-vars)															  \
																			  \
(let ((debug-on-error t)													  \
	  (url-show-status nil)													  \
	  (user-emacs-directory default-directory)								  \
	  (user-init-file (expand-file-name \"~/AppData/Roaming/build.el\"))	  \
	  (load-path (delq default-directory load-path)))						  \
  (load-file user-init-file)												  \
  (run-hooks (quote after-init-hook))										  \
  (run-hooks (quote emacs-startup-hook))))"

unix-startup:
	@echo "Test..."
	@$(EMACS) -Q --batch --eval $(UNIX-ENTRY)

windows-startup:
	@echo "Test..."
	@$(EMACS) -Q --batch --eval $(WINDOWS-ENTRY)
