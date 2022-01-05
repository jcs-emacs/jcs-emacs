#!/bin/sh -e

echo "Copying core files..."

mv -f ../jcs-emacs ~/.emacs.d

tree ~/.emacs.d

echo "Done copying configuration files"
