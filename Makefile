SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

.PHONY: startup

startup:
	@echo "Testing..."
	@$(EMACS) -nw --batch -l "~/.emacs.d/bin/test-startup.el"

speed:
	@echo "Speed testing..."
	@$(EMACS) -nw --batch -l "~/.emacs.d/bin/test-speed.el"
