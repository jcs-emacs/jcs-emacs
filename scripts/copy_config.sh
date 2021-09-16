#!/bin/sh -e

echo "Copying core files..."

mv -f ./build.el ~/build.el
mv -f ./.emacs ~/.emacs
mv -f ./.emacs.d ~/.emacs.d
mv -f ./.emacs.jcs ~/.emacs.jcs

echo "Done copying configuration files"
