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

(setq jcs-creator-name "Jen-Chieh Shen")
(setq jcs-copyright-name "Shen, Jen-Chieh")

(defun jcs-insert-creator-name ()
  "Insert the creator name."
  (interactive)
  (insert jcs-creator-name)
  )

(defun jcs-insert-copyright-name ()
  "Insert the copyright name."
  (interactive)
  (insert jcs-copyright-name)
  )


(defun jcs-insert-filename-section ()
  "Insert 'File' section."
  (interactive)

  ;; macro
  ;; file name
  (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  ;; file name with extension
  (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

  (insert "$File: ")
  (insert BaseFileNameWithExtension)
  )

(defun jcs-insert-creation-date-section ()
  "Insert 'Creation Date' section."
  (interactive)

  (insert "$Date: ")
  (jcs-timestamp)
  )

(defun jcs-insert-revision-section ()
  "Insert 'Revision' section."
  (interactive)

  (insert "$Revision: ")

  ;; TODO(jenchieh): insert revision design here...

  ;; NOTE(jenchieh): Because now the design is empty we just
  ;; delete the a whitespace before, so make the file look
  ;; consistent.
  (backward-delete-char 1)
  )


(defun jcs-insert-creator-section ()
  "Insert 'Creator' section."
  (interactive)

  (insert "$Creator: ")
  (jcs-insert-creator-name)
  )

(defun jcs-insert-notice-section-line1 ()
  "Insert 'Notice' section line 1."
  (interactive)

  (insert "$Notice: See LICENSE.txt for modification and distribution information")
  )

(defun jcs-insert-notice-section-line2 ()
  "Insert 'Notice' section line 2."
  (interactive)

  (insert "                  Copyright (c) ")
  (jcs-year-only)
  (insert " by ")
  (jcs-insert-copyright-name)
  )

;;---------------------------------------------
;; Full File info design here...
;; general comment style.
;;---------------------------------------------
(defun jcs-global-file-info ()
  ""
  (interactive)

  (insert "/**\n")
  (insert " * ")
  (jcs-insert-filename-section)
  (insert " $\n")
  (insert " * ")
  (jcs-insert-creation-date-section)
  (insert " $\n")
  (insert " * ")
  (jcs-insert-revision-section)
  (insert " $\n")
  (insert " * ")
  (jcs-insert-creator-section)
  (insert " $\n")
  (insert " * ")
  (jcs-insert-notice-section-line1)
  (insert " \n")
  (insert " * ")
  (jcs-insert-notice-section-line2)
  (insert " $\n")
  (insert " */\n")
  )

;;---------------------------------------------
;; Tag file comment style
;;---------------------------------------------
(defun jcs-tag-file-info ()
  ""
  (interactive)

  (insert "<!--\n")
  (insert "   - ")
  (jcs-insert-filename-section)
  (insert " $\n")
  (insert "   - ")
  (jcs-insert-creation-date-section)
  (insert " $\n")
  (insert "   - ")
  (jcs-insert-revision-section)
  (insert " $\n")
  (insert "   - ")
  (jcs-insert-creator-section)
  (insert " $\n")
  (insert "   - ")
  (jcs-insert-notice-section-line1)
  (insert " \n")
  (insert "   - ")
  (jcs-insert-notice-section-line2)
  (insert " $\n")
  (insert "   -->\n")
  (insert "\n\n")
  )

;;---------------------------------------------
;; Manage file comment style
;;---------------------------------------------
(defun jcs-manage-file-info ()
  "Any managing file format. Text file, batch file, shell
script, etc."
  (interactive)

  (insert "# ========================================================================\n")
  (insert "# ")
  (jcs-insert-filename-section)
  (insert " $\n")
  (insert "# ")
  (jcs-insert-creation-date-section)
  (insert " $\n")
  (insert "# ")
  (jcs-insert-revision-section)
  (insert " $\n")
  (insert "# ")
  (jcs-insert-creator-section)
  (insert " $\n")
  (insert "# ")
  (jcs-insert-notice-section-line1)
  (insert " \n")
  (insert "# ")
  (jcs-insert-notice-section-line2)
  (insert " $\n")
  (insert "# ========================================================================\n")
  (insert "\n\n")
  )

;;---------------------------------------------
;; Asm file comment style
;;---------------------------------------------
(defun jcs-asm-file-format ()
  "Specific header format for Assembly Language/lisp/elisp, etc."
  (interactive)

  (insert ";; ========================================================================\n")
  (insert ";; ")
  (jcs-insert-filename-section)
  (insert " $\n")
  (insert ";; ")
  (jcs-insert-creation-date-section)
  (insert " $\n")
  (insert ";; ")
  (jcs-insert-revision-section)
  (insert " $\n")
  (insert ";; ")
  (jcs-insert-creator-section)
  (insert " $\n")
  (insert ";; ")
  (jcs-insert-notice-section-line1)
  (insert " \n")
  (insert ";; ")
  (jcs-insert-notice-section-line2)
  (insert " $\n")
  (insert ";; ========================================================================\n")
  (insert "\n\n")
  )


;;---------------------------------------------
;; Specialize the makefile format more specific.
;;---------------------------------------------
(defun jcs-makefile-format-info ()
  "File header format specific for makefile."
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

  (insert "\n# General\n")
  (insert "VER        = 1.0.1         # version number\n")
  (insert "ROOT_DIR   = ..\n")
  (insert "BUILD_NAME = build_name    # enter the name of the build file.\n")
  (insert "                           # could either be a dynamic link, executable, etc.\n")

  (insert "\n# Commands\n")
  (insert "ASM  = nasm          # assembler type\n")
  (insert "DAMS = objdump       # disassembler commands\n")
  (insert "CC   = gcc           # compiler type\n")
  (insert "LD   = ld            # linker commands\n")
  (insert "AR   = ar            # compile lib file commands\n")

  (insert "\n# Flags\n")
  (insert "ASM_FLAGS     =            # assemble flags\n")
  (insert "DASM_FLAGS    = -D         # disassemble flags\n")
  (insert "C_FLAGS       = -Wall      # compile flags\n")
  (insert "LD_FLAGS      = -L         # linker flags\n")
  (insert "INCLUDE_FLAGS = -I         # include flags\n")
  (insert "AR_FLAGS      = rcs        # static link flags\n")
  (insert "SOR_FLAGS     = -shared    # dynamic link flags\n")
  (insert "OUTPUT_FLAGS  = -o         # output flags\n")

  (insert "\n# Library File\n")
  (insert "ALIB  = default_static_lib_name.a      # static link library\n")
  (insert "SOLIB = default_shared_lib_name.so     # dynamic link library\n")

  (insert "\n# Source Path\n")
  (insert "SOURCE_PATH     = $(ROOT_DIR)/src\n")

  (insert "\n# Include Path\n")
  (insert "INCLUDE_PATH   = $(ROOT_DIR)/include\n")
  (insert "INCLUDE_PATHS  = $(wildcard $(INCLUDE_PATH)/*)\n")

  (insert "\n# Library path\n")
  (insert "STATIC_LIB_PATH  = $(ROOT_DIR)/lib/alib\n")
  (insert "STATIC_LIB_PATHS = $(wildcard $(STATIC_LIB_PATH)/*)\n\n")

  (insert "SHARED_LIB_PATH  := $(ROOT_DIR)/lib/solib\n")
  (insert "SHARED_LIB_PATHS := $(wildcard $(SHARED_LIB_PATH)/*)\n")

  (insert "\n# All Source\n")
  (insert "ASMSRC := $(wildcard $(SOURCE_PATH)/*.asm)                           # asm source\n")
  (insert "GSRC   := $(wildcard $(SOURCE_PATH)/*.c $(SOURCE_PATH)/*.cpp)        # c/c++ source\n")
  (insert "ASRC   := $(wildcard $(STATIC_LIB_PATH)/*.c $(SOURCE_PATH)/*.cpp)    # static link library\n")
  (insert "SOSRC  := $(wildcard $(SHARED_LIB_PATH)/*.c $(SOURCE_PATH)/*.cpp)    # shared link library\n")

  (insert "\n# objs\n")
  (insert "OBJS   := $(patsubst %.c,%.o, $(patsubst %.cpp,%.o, $(GSRC)))     # list of object files\n")
  (insert "AOBJS  := $(patsubst %.c,%.o, $(patsubst %.cpp,%.o, $(ASRC)))     # .a object files\n")
  (insert "SOOBJS := $(patsubst %.c,%.o, $(patsubst %.cpp,%.o, $(SOSRC)))    # .so object files\n")

  (insert "\n# ASM objs\n")
  (insert "ASMOBJS := $(subst .asm,.o,$(ASMSRC))\n")

  (insert "\n# Dependencies\n")
  (insert "DEPDIR := $(ROOT_DIR)/mkdep")
  (insert "GDEP   := $(patsubst %.c,$(DEPDIR)/%.d,$(patsubst %.cpp,$(DEPDIR)/%.d, $(GSRC)))\n")
  (insert "ASMDEP := $(patsubst %.asm,$(DEPDIR)/%.d,$(ASMSRC))\n")

  (insert "\n\n.PHONY : build compile clean realclean\n\n")

  (insert "build : \n")
  (insert "    $(CC) $(GSRC) \\n")
  (insert "    $(INCLUDE_FLAGS) $(INCLUDE_PATHS) \\n")
  (insert "    $(LD_FLAGS) $(STATIC_LIB_PATHS) \\n")
  (insert "    $(OUTPUT_FLAGS) $(ROOT_DIR)/build/$(BUILD_NAME)")

  (insert "\n# compile all the source file to object file.\n")
  (insert "compile : $(OBJS) $(AOBJS) $(SOOBJS)\n")

  (insert "\n# Clean the project.\n")
  (insert "clean :\n")
  (insert "    rm -f %(OBJS) $(LOBJS)\n")

  (insert "realclean :\n")
  (insert "    rm -f $(OBJS) $(LOBJS) $(KASMOBJS) $(LASMOBJS) $(ALIB) $(SOLIB)\n")

  (insert "\n# include dependencies.\n")
  (insert "-include $(GDEP)\n")
  (insert "-include $(ASMDEP)\n")

  (insert "\n# example of compile the program main file.\n")
  (insert "program_main.o : program_main.c\n")
  (insert "    $(CC) $(C_FLAGS) $(OUTPUT_FLAGS) $@ $<\n")

  (insert "\n# generate static link library.\n")
  (insert "$(ALIB) : $(AOBJS)\n")
  (insert "    $(AR) $(AR_FLAGS) $@ $<\n")

  (insert "\n# generate shared link library.\n")
  (insert "$(SOLIB) : $(SOOBJS)\n")
  (insert "    $(CC) $(SOR_FLAGS) $@ $<\n")
  )

;;---------------------------------------------
;; Specialize the CMakeLists to more specific.
;;---------------------------------------------
(defun jcs-cmake-format-info ()
  "CMake file format info."
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

;;---------------------------------------------
;; Lua file header format.
;;---------------------------------------------
(defun jcs-lua-file-format-info ()
  "Lua file header format."
  (interactive)

  (insert "-- ========================================================================\n")
  (insert "-- ")
  (jcs-insert-filename-section)
  (insert " $\n")
  (insert "-- ")
  (jcs-insert-creation-date-section)
  (insert " $\n")
  (insert "-- ")
  (jcs-insert-revision-section)
  (insert " $\n")
  (insert "-- ")
  (jcs-insert-creator-section)
  (insert " $\n")
  (insert "-- ")
  (jcs-insert-notice-section-line1)
  (insert " \n")
  (insert "-- ")
  (jcs-insert-notice-section-line2)
  (insert " $\n")
  (insert "-- ========================================================================\n")
  (insert "\n\n")
  )

;;---------------------------------------------
;; Batch file header format.
;;---------------------------------------------
(defun jcs-batch-file-format-info ()
  "Header format for batch file."
  (interactive)

  (insert ":: ========================================================================\n")
  (insert ":: ")
  (jcs-insert-filename-section)
  (insert " $\n")
  (insert ":: ")
  (jcs-insert-creation-date-section)
  (insert " $\n")
  (insert ":: ")
  (jcs-insert-revision-section)
  (insert " $\n")
  (insert ":: ")
  (jcs-insert-creator-section)
  (insert " $\n")
  (insert ":: ")
  (jcs-insert-notice-section-line1)
  (insert " \n")
  (insert ":: ")
  (jcs-insert-notice-section-line2)
  (insert " $\n")
  (insert ":: ========================================================================\n")
  (insert "\n\n")
  )

(defun jcs-cobol-file-format-info ()
  "Header format for COBOL."

  (insert "       *> ========================================================================\n")
  (insert "       *> ")
  (jcs-insert-filename-section)
  (insert " $\n")
  (insert "       *> ")
  (jcs-insert-creation-date-section)
  (insert " $\n")
  (insert "       *> ")
  (jcs-insert-revision-section)
  (insert " $\n")
  (insert "       *> ")
  (jcs-insert-creator-section)
  (insert " $\n")
  (insert "       *> ")
  (jcs-insert-notice-section-line1)
  (insert " \n")
  (insert "       *> ")
  (jcs-insert-notice-section-line2)
  (insert " $\n")
  (insert "       *> ========================================================================\n")
  (insert "\n\n")
  )

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-file-info-format.el file
