SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

.PHONY: clean install compile startup mode speed packages

clean:
	@echo "Cleaning..."
	$(EASK) clean-all

copy_config:
	@echo "Copying configuration..."
	./test/startup/copy_config.sh

install:
	@echo "Installing dependenices..."
	$(EASK) install-deps

compile:
	@echo "Compiling..."
	$(EASK) concat
	$(EASK) load ./test/test-compile.el

startup:
	@echo "Startup testing..."
	$(EASK) refresh -g
	$(EMACS) -q --batch -l "./test/startup/test-startup.el"

mode:
	@echo "Testing Major Modes..."
	$(EASK) refresh -g
	$(EMACS) -q --batch -l "./test/test-mode.el"

speed:
	@echo "Speed testing..."
	$(EASK) refresh -g
	$(EMACS) -q --batch -l "./test/test-speed.el"

packages:
	@echo "Packages..."
	$(EASK) refresh -g
	$(EMACS) -q --batch -l "./test/test-packages.el"
