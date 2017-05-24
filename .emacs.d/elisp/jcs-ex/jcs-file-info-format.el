;; This is the start of jcs-file-info-format.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-file-info-format.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2016 Jen-Chieh Shen

;; jcs-file-info-format is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-file-info-format is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;========================================
;;      JENCHIEH KEY GLOBAL INFO
;;----------------------------------

;;---------------------------------------------
;; Full File info design here...
;; general comment style.
;;---------------------------------------------
(defun jcs-global-file-info ()

  (interactive)

  ;; macro
  ;; file name
  (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  ;; file name with extension
  (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

  (insert "/**\n")
  (insert " * $File: ")
  (insert BaseFileNameWithExtension)
  (insert " $\n")
  (insert " * $Date: ")
  (jcs-timestamp)
  (insert " $\n")
  (insert " * $Revision: $\n")
  (insert " * $Creator: Jen-Chieh Shen $\n")
  (insert " * $Notice: See LICENSE.txt for modification and distribution information \n")
  (insert " *                   Copyright (c) ")
  (jcs-year-only)
  (insert " by Shen, Jen-Chieh $\n")
  (insert " */\n")
  )

;;---------------------------------------------
;; Tag file comment style
;;---------------------------------------------
(defun jcs-tag-file-info ()

  (interactive)

  ;; macro
  (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

  (insert "<!--\n")
  (insert "   - $File: ")
  (insert BaseFileNameWithExtension)
  (insert " $\n")
  (insert "   - $Date: ")
  (jcs-timestamp)
  (insert " $\n")
  (insert "   - $Revision: $\n")
  (insert "   - $Creator: Jen-Chieh Shen $\n")
  (insert "   - $Notice: See LICENSE.txt for modification and distribution information \n")
  (insert "   -                   Copyright (c) ")
  (jcs-year-only)
  (insert " by Shen, Jen-Chieh $\n")
  (insert "   -->\n")
  (insert "\n\n")
  )

;;---------------------------------------------
;; Manage file comment style
;;---------------------------------------------
(defun jcs-manage-file-info ()

  (interactive)

  ;; macro
  (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

  (insert "#  ========================================================================\n")
  (insert "#  $File: ")
  (insert BaseFileNameWithExtension)
  (insert " $\n")
  (insert "#  $Date: ")
  (jcs-timestamp)
  (insert " $\n")
  (insert "#  $Revision: $\n")
  (insert "#  $Creator: Jen-Chieh Shen $\n")
  (insert "#  $Notice: See LICENSE.txt for modification and distribution information \n")
  (insert "#                    Copyright (c) ")
  (jcs-year-only)
  (insert " by Shen, Jen-Chieh $\n")
  (insert "#  ========================================================================\n")
  (insert "\n\n")
  )

;;---------------------------------------------
;; Asm file comment style
;;---------------------------------------------
(defun jcs-asm-file-format ()
  (interactive)

  ;; macro
  (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

  (insert ";; ========================================================================\n")
  (insert ";; $File: ")
  (insert BaseFileNameWithExtension)
  (insert " $\n")
  (insert ";; $Date: ")
  (jcs-timestamp)
  (insert " $\n")
  (insert ";; $Revision: $\n")
  (insert ";; $Creator: Jen-Chieh Shen $\n")
  (insert ";; $Notice: See LICENSE.txt for modification and distribution information \n")
  (insert ";;                   Copyright (c) ")
  (jcs-year-only)
  (insert " by Shen, Jen-Chieh $\n")
  (insert ";; ========================================================================\n")
  (insert "\n\n")
  )


;;---------------------------------------------
;; Specialize the makefile format more specific.
;;---------------------------------------------
(defun jcs-makefile-format-info ()

  (interactive)

  (insert "#----------------------------------------------\n")
  (insert "# JayCeS project directories preference.\n")
  (insert "#----------------------------------------------\n")
  (insert "# .\n")
  (insert "# ├── build\n")
  (insert "# ├── data\n")
  (insert "# ├── doc\n")
  (insert "# ├── lib\n")
  (insert "# │   ├── alib\n")
  (insert "# │   └── solib\n")
  (insert "# ├── misc\n")
  (insert "# └── src\n")
  (insert "#----------------------------------------------\n")

  (insert "\n# general settings\n")
  (insert "VER = 1.0.1                  # version number\n")
  (insert "ROOT_DIR = ..\n")
  (insert "BUILD_NAME = build_name      # enter the name of the build file.\n")
  (insert "                             # could either be a dynamic link, executable, etc.\n")

  (insert "\n# commands\n")
  (insert "ASM = nasm         # assembler type\n")
  (insert "DAMS = objdump     # disassembler commands\n")
  (insert "CC = gcc           # compiler type\n")
  (insert "LD = ld            # linker commands\n")
  (insert "AR = ar            # compile lib file commands\n")

  (insert "\n# flags\n")
  (insert "ASMFLAGS =                         # assemble flags\n")
  (insert "DASMFLAGS = -D                     # disassemble flags\n")
  (insert "CFLAGS = -Wall                     # compile flags\n")
  (insert "LDFALGS =                          # linker flags\n")
  (insert "ARFLAGS = rcs                      # static link flags\n")
  (insert "SORFLAGS = -shared                 # dynamic link flags\n")

  (insert "\n# library file\n")
  (insert "ALIB = default_static_lib_name.a       # static link library\n")
  (insert "SOLIB = default_shared_lib_name.so     # dynamic link library\n")

  (insert "\n# path setting\n")
  (insert "SOURCE_PATH = $(ROOT_DIR)/src\n")
  (insert "STATIC_LIB_PATH = $(ROOT_DIR)/lib/alib\n")
  (insert "SHARED_LIB_PATH = $(ROOT_DIR)/lib/solib\n")

  (insert "\n# source\n")
  (insert "GSRC = $(wildcard $(SOURCE_PATH)/*.c)            # general source\n")
  (insert "ASRC = $(wildcard $(STATIC_LIB_PATH)/*.c)        # static link library\n")
  (insert "SOSRC = $(wildcard $(SHARED_LIB_PATH)/*.c)       # shared link library\n")

  (insert "\n# object file\n")
  (insert "OBJS = (subst .c,.o, $(GSRC))          # list of object files\n")
  (insert "AOBJS = (subst .c,.o, $(ASRC))\n")
  (insert "SOOBJS = (subst .c,.o, $(SOSRC))\n")

  (insert "\n# dependencies\n")
  (insert "DEPDIR = .mkdep")
  (insert "GDEP = $(patsubst %.c,$(DEPDIR)/%.d,$(GSRC))\n")


  (insert "\n\n.PHONY : build compile\n\n")

  (insert "build : \n")
  (insert "    $(CC) $(GSRC) -o $(ROOT_DIR)/build/$(BUILD_NAME)\n")

  (insert "\n# compile all the source file to object file.\n")
  (insert "compile : $(OBJS) $(AOBJS) $(SOOBJS)\n")

  (insert "\n# example of compile the program main file.\n")
  (insert "program_main.o : program_main.c\n")
  (insert "    $(CC) $(CFLAGS) -o $@ $<\n")

  (insert "\n# generate static link library.\n")
  (insert "$(ALIB) : $(AOBJS)\n")
  (insert "    $(AR) $(ARFLAGS) $@ $<\n")

  (insert "\n# generate shared link library.\n")
  (insert "$(SOLIB) : $(SOOBJS)\n")
  (insert "    $(CC) $(SORFLAGS) $@ $<\n")
  )

;;---------------------------------------------
;; Specialize the CMakeLists to more specific.
;;---------------------------------------------
(defun jcs-cmake-format-info ()

  (interactive)

  (insert "CMAKE_MINIMUM_REQUIRED(VERSION 3.0)\n\n")

  (insert "# project settings\n")
  (insert "SET(VERSION_MAJOR \"1\")\n")
  (insert "SET(VERSION_MINOR \"0\")\n")
  (insert "SET(VERSION_PATCH \"0\")\n")
  (insert "SET(VERSION \"${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}\")\n\n")

  (insert "# environment settings\n")
  (insert "ADD_DEFINITIONS(-DUNICODE)\n")
  (insert "ADD_DEFINITIONS(-D_UNICODE)\n\n")

  (insert "# subdir settings\n")
  (insert "ADD_SUBDIRECTORY(libssrc)\n")
  )

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-file-info-format.el file
