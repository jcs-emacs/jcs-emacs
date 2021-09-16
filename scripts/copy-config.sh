#!/bin/sh -e

echo "Copying core files..."

cp ./build.el ~/build.el
cp ./.emacs ~/.emacs
cp ./.emacs.d ~/.emacs.d
cp ./.emacs.jcs ~/.emacs.jcs

echo "Done copying configuration files"
