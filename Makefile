SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

.PHONY: clean startup speed compile

clean:
	@echo "Cleaning..."
	$(EASK) clean-all

install:
	@echo "Installing dependenices..."
	$(EASK) install-deps

startup:
	make install -g
	@echo "Startup testing..."
	$(EMACS) -nw --batch -l "~/.emacs.d/test/startup/test-startup.el"

speed:
	@echo "Speed testing..."
	$(EMACS) -nw --batch -l "~/.emacs.d/test/startup/test-speed.el"

compile:
	@echo "Compiling..."
	$(EASK) concat
	$(EASK) load ./test/test-compile.el
