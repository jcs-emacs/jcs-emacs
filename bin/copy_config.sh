#!/bin/sh -e

echo "Copying core files..."

mkdir ~/.emacs.d
mv -f ./.emacs.d ~/.emacs.d

echo "Done copying configuration files"
