@echo off
:: ========================================================================
:: $File: install.bat $
:: $Date: 2019-03-28 21:16:32 $
:: $Revision: $
:: $Creator: Jen-Chieh Shen $
:: $Notice: See LICENSE.txt for modification and distribution information
::                   Copyright © 2019 by Shen, Jen-Chieh $
:: ========================================================================

:: DESCRIPTION: Install this configuration onto this machine.


set INSTALL_DIR=%userprofile%\AppData\Roaming

:: To root directory.
cd ..

:: copy init file to home
xcopy ".emacs" "%INSTALL_DIR%"

:: create necessary directories.
mkdir "%INSTALL_DIR%\.emacs.d"
mkdir "%INSTALL_DIR%\.emacs.jcs"

:: copy core directories to home
xcopy /e ".emacs.d" "%INSTALL_DIR%\.emacs.d"
xcopy /e ".emacs.jcs" "%INSTALL_DIR%\.emacs.jcs"
