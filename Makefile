SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

.PHONY: startup

startup:
	@$(EMACS) -nw --batch -l "~/.emacs.d/bin/test-startup.el"

speed:
	@$(EMACS) -nw --batch -l "~/.emacs.d/bin/test-speed.el"
