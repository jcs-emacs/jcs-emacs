@echo off
:: ========================================================================
:: $File: open_emacs.bat $
:: $Date: 2017-11-15 11:53:53 $
:: $Revision: $
:: $Creator: Jen-Chieh Shen $
:: $Notice: See LICENSE.txt for modification and distribution information
::                   Copyright (c) 2017 by Shen, Jen-Chieh $
:: ========================================================================


set EMACS_INIT_PATH=
set EMACS_PROGRAM_PATH=


:: Run emacs with initialize setting file.
%EMACS_PROGRAM_PATH% %EMACS_INIT_PATH%
