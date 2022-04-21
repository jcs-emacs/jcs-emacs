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
	$(EMACS) -q --batch -l "./test/startup/test-startup.el"

mode:
	@echo "Testing Major Modes..."
	$(EMACS) -q --batch -l "./test/test-mode.el"

speed:
	@echo "Speed testing..."
	$(EMACS) -q --batch -l "./test/test-speed.el"

packages:
	@echo "Packages..."
	$(EMACS) -q --batch -l "./test/test-packages.el"
