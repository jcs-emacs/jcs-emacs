SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

.PHONY: startup speed compile

startup:
	@echo "Testing..."
	$(EMACS) -nw --batch -l "~/.emacs.d/bin/test-startup.el"

speed:
	@echo "Speed testing..."
	$(EMACS) -nw --batch -l "~/.emacs.d/bin/test-speed.el"

compile:
	@echo "Compiling..."
	$(EASK) concat
	$(EASK) load ./bin/test-compile.el
