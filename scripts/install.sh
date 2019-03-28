#!/bin/bash
# ========================================================================
# $File: install.sh $
# $Date: 2019-03-28 21:15:01 $
# $Revision: $
# $Creator: Jen-Chieh Shen $
# $Notice: See LICENSE.txt for modification and distribution information
#                   Copyright © 2019 by Shen, Jen-Chieh $
# ========================================================================


# DESCRIPTION(jenchieh): Install this configuration onto this machine.

# To root directory.
cd ..

# copy init file to home
cp ./.emacs ~/

# copy core directories to home
cp -r ./.emacs.d ~/
cp -r ./.emacs.jcs ~/
