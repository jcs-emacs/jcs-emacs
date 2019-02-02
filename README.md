# jcs-emacs-init #

[![Build Status](https://travis-ci.com/jcs090218/jcs-emacs-init.svg?branch=master)](https://travis-ci.com/jcs090218/jcs-emacs-init)
[![License](https://img.shields.io/badge/License-BSD%202--Clause-orange.svg)](https://opensource.org/licenses/BSD-2-Clause)

This is Jen-Chieh Shen's emacs configuration, repeatedly utilized 
and modified since 2015, hopefully this could help someone who is 
new or struggle with Emacs. This configuration should works on 
most OSs, indicates Windows, Linux and MacOS.
<br/>

Emacs itself supports multiple programming languages. I managed 
all the programming languages I personally used. Indeed, I am 
a penchant to numerous technologies, incorporate hardware, firmware 
and software. Here is the list of programming languages I know 
and it has extended from this config.

* ActionScript 2.0 or 3.0 / Assembly Language
* BASIC / Batchfile
* C / C++ / C# / Clojure / COBOL / CSS
* Elisp
* Go
* Haskell / Haxe / HTML
* JSON / Java / JavaScript
* Lisp / Lua
* Makefile
* Objective-C
* Perl / PHP / Python
* Ruby / Rust
* Scala / Shell / SQL / Swift
* TypeScript
* Verilog / Vim script
* XML
* YAML

This configuration polished and goes toward the to the modern 
text editor, e.g. Atom, Sublime Text 2/3, VS Code, etc. 


## Features ##

* Autocompletion is powered by 
[auto-complete](https://github.com/auto-complete/auto-complete).
* Syntax Check is powered by 
[flycheck](http://www.flycheck.org/en/latest/).


## Installation ##
To install, clone this repo and copy the core files/directories 
to the emacs config directory accordingly.
```sh
# clone this repo
git clone https://github.com/jcs090218/jcs-emacs-init.git

# change current directory to project directory
cd jcs-emacs-init

# copy init file to home
cp ./.emacs ~/

# copy core directories to home
cp -r ./.emacs.d ~/
cp -r ./.emacs.jcs ~/
```


## Supported Emacs versions ##
The config should run on Emacs 24.3 or higher, but still, recommend 
to always run on the latest Emacs version available on your machine.
<br/>
