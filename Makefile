SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

.PHONY: startup

startup:
	@echo "What..."
	@$(EMACS) -nw --batch -l "~/.emacs.d/bin/test.el"
