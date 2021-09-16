#!/bin/sh -e

echo "Copying core files..."

cp -f ./build.el ~/build.el
cp -f ./.emacs ~/.emacs
cp -f ./.emacs.d ~/.emacs.d
cp -f ./.emacs.jcs ~/.emacs.jcs

echo "Done copying configuration files"
