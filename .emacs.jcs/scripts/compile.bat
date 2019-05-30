@echo off
:: ========================================================================
:: $File: compile.bat $
:: $Date: 2019-05-30 11:28:58 $
:: $Revision: $
:: $Creator: Jen-Chieh Shen $
:: $Notice: See LICENSE.txt for modification and distribution information
::                   Copyright © 2019 by Shen, Jen-Chieh $
:: ========================================================================


cd ..

emacs --batch --eval "(byte-recompile-directory \"./\" 0)"
