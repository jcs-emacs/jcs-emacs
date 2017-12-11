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
  (insert jcs-creator-name)
  )

(defun jcs-insert-copyright-name ()
  "Insert the copyright name."
  (insert jcs-copyright-name)
  )


(defun jcs-insert-filename-section ()
  "Insert 'File' section."

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

  (insert "$Date: ")
  (jcs-timestamp)
  )

(defun jcs-insert-revision-section ()
  "Insert 'Revision' section."

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

  (insert "$Notice: See LICENSE.txt for modification and distribution information")
  )

(defun jcs-insert-notice-section-line2 ()
  "Insert 'Notice' section line 2."

  (insert "                  Copyright © ")
  (jcs-year-only)
  (insert " by ")
  (jcs-insert-copyright-name)
  )

;;---------------------------------------------
;; Full File info design here...
;; general comment style.
;;---------------------------------------------
(defun jcs-global-file-info ()
  "Useing '/*' '*/' for commenting programming languages."

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
  (call-interactively 'jcs-ask-makefile-app-template))

(defun jcs-makefile-app-template ()
  "Default makefile template for normal application."

  (insert "### Application Makefile Template ###\n\n")

  (insert "#----------------------------------------------\n")
  (insert "# JayCeS project directories preference.\n")
  (insert "#----------------------------------------------\n")
  (insert "# .\n")
  (insert "# ├── build\n")
  (insert "# │   ├── alib\n")
  (insert "# │   └── bin\n")
  (insert "# │   └── solib\n")
  (insert "# ├── data\n")
  (insert "# ├── doc\n")
  (insert "# ├── lib\n")
  (insert "# │   ├── alib\n")
  (insert "# │   └── solib\n")
  (insert "# ├── misc\n")
  (insert "# ├── src\n")
  (insert "# └── test\n")
  (insert "#----------------------------------------------\n")

  (insert "\n### General ###\n")
  (insert "# version number\n")
  (insert "VER        = 1.0.1\n")
  (insert "ROOT_DIR   = .\n")
  (insert "# Enter the name of the build file. could either be a dynamic\n")
  (insert "# link, executable, etc.\n")
  (insert "BIN_NAME = bin_name\n")

  (insert "\n### Directories ###\n")
  (insert "# Build executable directory.\n")
  (insert "BIN_DIR = $(ROOT_DIR)/build/bin\n")
  (insert "# Build library directory.\n")
  (insert "ALIB_DIR = $(ROOT_DIR)/build/alib\n")
  (insert "# Build library directory.\n")
  (insert "SOLIB_DIR = $(ROOT_DIR)/build/solib\n")

  (insert "\n### Commands ###\n")
  (insert "# assembler type\n")
  (insert "ASM  = nasm\n")
  (insert "# disassembler commands\n")
  (insert "DAMS = objdump\n")
  (insert "# compiler type\n")
  (insert "CC   = gcc\n")
  (insert "# linker commands\n")
  (insert "LD   = ld\n")
  (insert "# compile lib file commands\n")
  (insert "AR   = ar\n")

  (insert "\n### Flags ##\n")
  (insert "# assemble flags\n")
  (insert "ASM_FLAGS     = \n")
  (insert "# disassemble flags\n")
  (insert "DASM_FLAGS    = -D\n")
  (insert "# compile flags\n")
  (insert "C_FLAGS       = -Wall\n")
  (insert "# linker flags\n")
  (insert "LD_FLAGS      = -L\n")
  (insert "# include flags\n")
  (insert "INCLUDE_FLAGS = -I\n")
  (insert "# static link flags\n")
  (insert "AR_FLAGS      = rcs\n")
  (insert "# dynamic link flags\n")
  (insert "SOR_FLAGS     = -shared\n")
  (insert "# output flags\n")
  (insert "OUTPUT_FLAGS  = -o\n")

  (insert "\n### Library File ###\n")
  (insert "# static link library\n")
  (insert "ALIB  = a_lib_name.a\n")
  (insert "# dynamic link library\n")
  (insert "SOLIB = so_lib_name.so\n")

  (insert "\n### Source Path ###\n")
  (insert "MAIN_PATH       = $(ROOT_DIR)/test\n")
  (insert "SOURCE_PATH     = $(ROOT_DIR)/src\n")

  (insert "\n### Include Path ###\n")
  (insert "INCLUDE_PATH   = $(ROOT_DIR)/include\n")

  (insert "\n### Library path ###\n")
  (insert "A_LIB_PATH  = $(ROOT_DIR)/lib/alib\n")
  (insert "A_LIBS = $(wildcard $(A_LIB_PATH)/*)\n\n")

  (insert "SO_LIB_PATH  := $(ROOT_DIR)/lib/solib\n")
  (insert "SO_LIBS := $(wildcard $(SO_LIB_PATH)/*)\n")

  (insert "\n### All Source ###\n")
  (insert "# main source\n")
  (insert "MAINSRC := $(sort $(wildcard $(MAIN_PATH)/*.asm $(MAIN_PATH)/*.c $(MAIN_PATH)/*.cpp))\n")
  (insert "# asm source\n")
  (insert "ASMSRC  := $(sort $(wildcard $(SOURCE_PATH)/*.asm))\n")
  (insert "# c/c++ source\n")
  (insert "GSRC    := $(sort $(wildcard $(SOURCE_PATH)/*.c $(SOURCE_PATH)/*.cpp))\n")
  (insert "# static link library source\n")
  (insert "ASRC    := $(sort $(wildcard $(SOURCE_PATH)/*.c $(SOURCE_PATH)/*.cpp $(A_LIB_PATH)/*.c $(A_LIB_PATH)/*.cpp))\n")
  (insert "# shared link library source\n")
  (insert "SOSRC   := $(sort $(wildcard $(SOURCE_PATH)/*.c $(SOURCE_PATH)/*.cpp $(SO_LIB_PATH)/*.c $(SO_LIB_PATH)/*.cpp))\n")

  (insert "\n### objs ###\n")
  (insert "# main object file\n")
  (insert "MAINOBJ := $(sort $(patsubst %.c,%.o, $(patsubst %.cpp,%.o, $(MAINSRC))))\n")
  (insert "# list of object files\n")
  (insert "OBJS    := $(sort $(patsubst %.c,%.o, $(patsubst %.cpp,%.o, $(GSRC))))\n")
  (insert "# .a object files\n")
  (insert "AOBJS   := $(sort $(patsubst %.c,%.o, $(patsubst %.cpp,%.o, $(ASRC))))\n")
  (insert "# .so object files\n")
  (insert "SOOBJS  := $(sort $(patsubst %.c,%.o, $(patsubst %.cpp,%.o, $(SOSRC))))\n")

  (insert "\n### ASM objs ###\n")
  (insert "ASMOBJS := $(sort $(subst .asm,.o,$(ASMSRC)))\n")

  (insert "\n### Dependencies ###\n")
  (insert "DEPDIR := $(ROOT_DIR)/mkdep")
  (insert "GDEP   := $(patsubst %.c,$(DEPDIR)/%.d,$(patsubst %.cpp,$(DEPDIR)/%.d, $(GSRC)))\n")
  (insert "ASMDEP := $(patsubst %.asm,$(DEPDIR)/%.d,$(ASMSRC))\n")


  (insert "\n\n.PHONY : build compile clean realclean test\n\n")

  (insert "test : \n")
  (insert "    echo \"Test command...\"\n\n")

  (insert "build : \n")
  (insert "    $(CC) $(GSRC) $(MAINSRC) \\\n")
  (insert "    $(INCLUDE_FLAGS) $(INCLUDE_PATH) \\\n")
  (insert "    $(A_LIBS) \\\n")
  (insert "    $(SO_LIBS) \\\n")
  (insert "    $(LD_FLAGS) $(A_LIB_PATH) \\\n")
  (insert "    $(OUTPUT_FLAGS) $(BIN_DIR)/$(BIN_NAME)\n\n")

  (insert "\n# compile all the source file to object file.\n")
  (insert "compile : $(MAINOBJ) $(OBJS) $(AOBJS) $(SOOBJS)\n")

  (insert "\n# Clean the project.\n")
  (insert "clean :\n")
  (insert "    rm -f $(MAINOBJ) $(OBJS) $(LOBJS)\n\n")

  (insert "realclean :\n")
  (insert "    rm -f $(MAINOBJ) $(OBJS) $(LOBJS) $(KASMOBJS) $(LASMOBJS) $(ALIB_DIR)/$(ALIB) $(SOLIB_DIR)/$(SOLIB)\n")

  (insert "\n# include dependencies.\n")
  (insert "-include $(GDEP)\n")
  (insert "-include $(ASMDEP)\n")

  (insert "\n# example of compile the program main file.\n")
  (insert "program_main.o : program_main.c\n")
  (insert "    $(CC) $(C_FLAGS) $(OUTPUT_FLAGS) $@ $<\n")

  (insert "\n# generate static link library.\n")
  (insert "$(ALIB) : $(AOBJS)\n")
  (insert "    $(AR) $(AR_FLAGS) $(ALIB_DIR)/$@ $^\n")

  (insert "\n# generate shared link library.\n")
  (insert "$(SOLIB) : $(SOOBJS)\n")
  (insert "    $(CC) $(SOR_FLAGS) \\\n")
  (insert "    $(OUTPUT_FLAGS) $(SOLIB_DIR)/$@ $^\n")
  )

(defun jcs-makefile-lib-template ()
  "Library makefile template for static library or shared library."

  (insert "### Library Makefile Template ###\n\n")

  (insert "#----------------------------------------------\n")
  (insert "# JayCeS project directories preference.\n")
  (insert "#----------------------------------------------\n")
  (insert "# .\n")
  (insert "# ├── build\n")
  (insert "# │   ├── alib\n")
  (insert "# │   └── bin\n")
  (insert "# │   └── solib\n")
  (insert "# ├── data\n")
  (insert "# ├── doc\n")
  (insert "# ├── lib\n")
  (insert "# │   ├── alib\n")
  (insert "# │   └── solib\n")
  (insert "# ├── misc\n")
  (insert "# ├── src\n")
  (insert "# └── test\n")
  (insert "#----------------------------------------------\n")

  (insert "\n### General ###\n")
  (insert "# version number\n")
  (insert "VER        = 1.0.1\n")
  (insert "ROOT_DIR   = .\n")
  (insert "# Enter the name of the build file. could either be a dynamic\n")
  (insert "# link, executable, etc.\n")
  (insert "BIN_NAME = bin_name\n")

  (insert "\n### Directories ###\n")
  (insert "# Build executable directory.\n")
  (insert "BIN_DIR = $(ROOT_DIR)/build/bin\n")
  (insert "# Build library directory.\n")
  (insert "ALIB_DIR = $(ROOT_DIR)/build/alib\n")
  (insert "# Build library directory.\n")
  (insert "SOLIB_DIR = $(ROOT_DIR)/build/solib\n")

  (insert "\n### Commands ###\n")
  (insert "# assembler type\n")
  (insert "ASM  = nasm\n")
  (insert "# disassembler commands\n")
  (insert "DAMS = objdump\n")
  (insert "# compiler type\n")
  (insert "CC   = gcc\n")
  (insert "# linker commands\n")
  (insert "LD   = ld\n")
  (insert "# compile lib file commands\n")
  (insert "AR   = ar\n")

  (insert "\n### Flags ##\n")
  (insert "# assemble flags\n")
  (insert "ASM_FLAGS     = \n")
  (insert "# disassemble flags\n")
  (insert "DASM_FLAGS    = -D\n")
  (insert "# compile flags\n")
  (insert "C_FLAGS       = -Wall\n")
  (insert "# linker flags\n")
  (insert "LD_FLAGS      = -L\n")
  (insert "# include flags\n")
  (insert "INCLUDE_FLAGS = -I\n")
  (insert "# static link flags\n")
  (insert "AR_FLAGS      = rcs\n")
  (insert "# dynamic link flags\n")
  (insert "SOR_FLAGS     = -shared\n")
  (insert "# output flags\n")
  (insert "OUTPUT_FLAGS  = -o\n")

  (insert "\n### Library File ###\n")
  (insert "# static link library\n")
  (insert "ALIB  = a_lib_name.a\n")
  (insert "# dynamic link library\n")
  (insert "SOLIB = so_lib_name.so\n")

  (insert "\n### Source Path ###\n")
  (insert "MAIN_PATH       = $(ROOT_DIR)/test\n")
  (insert "SOURCE_PATH     = $(ROOT_DIR)/src\n")

  (insert "\n### Include Path ###\n")
  (insert "INCLUDE_PATH   = $(ROOT_DIR)/include\n")

  (insert "\n### Library path ###\n")
  (insert "A_LIB_PATH  = $(ROOT_DIR)/lib/alib\n")
  (insert "A_LIBS = $(wildcard $(A_LIB_PATH)/*)\n\n")

  (insert "SO_LIB_PATH  := $(ROOT_DIR)/lib/solib\n")
  (insert "SO_LIBS := $(wildcard $(SO_LIB_PATH)/*)\n")

  (insert "\n### All Source ###\n")
  (insert "# main source\n")
  (insert "MAINSRC := $(sort $(wildcard $(MAIN_PATH)/*.asm $(MAIN_PATH)/*.c $(MAIN_PATH)/*.cpp))\n")
  (insert "# asm source\n")
  (insert "ASMSRC  := $(sort $(wildcard $(SOURCE_PATH)/*.asm))\n")
  (insert "# c/c++ source\n")
  (insert "GSRC    := $(sort $(wildcard $(SOURCE_PATH)/*.c $(SOURCE_PATH)/*.cpp))\n")
  (insert "# static link library source\n")
  (insert "ASRC    := $(sort $(wildcard $(SOURCE_PATH)/*.c $(SOURCE_PATH)/*.cpp $(A_LIB_PATH)/*.c $(A_LIB_PATH)/*.cpp))\n")
  (insert "# shared link library source\n")
  (insert "SOSRC   := $(sort $(wildcard $(SOURCE_PATH)/*.c $(SOURCE_PATH)/*.cpp $(SO_LIB_PATH)/*.c $(SO_LIB_PATH)/*.cpp))\n")

  (insert "\n### objs ###\n")
  (insert "# main object file\n")
  (insert "MAINOBJ := $(sort $(patsubst %.c,%.o, $(patsubst %.cpp,%.o, $(MAINSRC))))\n")
  (insert "# list of object files\n")
  (insert "OBJS    := $(sort $(patsubst %.c,%.o, $(patsubst %.cpp,%.o, $(GSRC))))\n")
  (insert "# .a object files\n")
  (insert "AOBJS   := $(sort $(patsubst %.c,%.o, $(patsubst %.cpp,%.o, $(ASRC))))\n")
  (insert "# .so object files\n")
  (insert "SOOBJS  := $(sort $(patsubst %.c,%.o, $(patsubst %.cpp,%.o, $(SOSRC))))\n")

  (insert "\n### ASM objs ###\n")
  (insert "ASMOBJS := $(sort $(subst .asm,.o,$(ASMSRC)))\n")

  (insert "\n### Dependencies ###\n")
  (insert "DEPDIR := $(ROOT_DIR)/mkdep")
  (insert "GDEP   := $(patsubst %.c,$(DEPDIR)/%.d,$(patsubst %.cpp,$(DEPDIR)/%.d, $(GSRC)))\n")
  (insert "ASMDEP := $(patsubst %.asm,$(DEPDIR)/%.d,$(ASMSRC))\n")


  (insert "\n\n.PHONY : build compile clean realclean test\n\n")

  (insert "test : \n")
  (insert "    echo \"Test command...\"\n\n")

  (insert "build : \n")
  (insert "    $(CC) $(GSRC) $(MAINSRC) \\\n")
  (insert "    $(A_LIBS) \\\n")
  (insert "    $(SO_LIBS) \\\n")
  (insert "    $(INCLUDE_FLAGS) $(INCLUDE_PATH) \\\n")
  (insert "    $(LD_FLAGS) $(A_LIB_PATH) \\\n")
  (insert "    $(OUTPUT_FLAGS) $(BIN_DIR)/$(BIN_NAME)\n\n")

  (insert "\n# compile all the source file to object file.\n")
  (insert "compile : $(MAINOBJ) $(OBJS) $(AOBJS) $(SOOBJS)\n")

  (insert "\n# Clean the project.\n")
  (insert "clean :\n")
  (insert "    rm -f $(MAINOBJ) $(OBJS) $(LOBJS)\n\n")

  (insert "realclean :\n")
  (insert "    rm -f $(MAINOBJ) $(OBJS) $(LOBJS) $(KASMOBJS) $(LASMOBJS) $(ALIB_DIR)/$(ALIB) $(SOLIB_DIR)/$(SOLIB)\n")

  (insert "\n# include dependencies.\n")
  (insert "-include $(GDEP)\n")
  (insert "-include $(ASMDEP)\n")

  (insert "\n# example of compile the program main file.\n")
  (insert "program_main.o : program_main.c\n")
  (insert "    $(CC) $(C_FLAGS) $(OUTPUT_FLAGS) $@ $<\n")

  (insert "\n# generate static link library.\n")
  (insert "$(ALIB) : $(AOBJS)\n")
  (insert "    $(AR) $(AR_FLAGS) $(ALIB_DIR)/$@ $^\n")

  (insert "\n# generate shared link library.\n")
  (insert "$(SOLIB) : $(SOOBJS)\n")
  (insert "    $(CC) $(SOR_FLAGS) \\\n")
  (insert "    $(OUTPUT_FLAGS) $(SOLIB_DIR)/$@ $^\n")
  )

;;---------------------------------------------
;; Specialize the CMakeLists to more specific.
;;---------------------------------------------
(defun jcs-cmake-format-info ()
  "CMake file format info."

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

;;---------------------------------------------
;; C Header file format.
;;---------------------------------------------
(defun jcs-c-header-file-format-info ()
  "Header for C header file."
  (jcs-global-file-info)
  )

;;---------------------------------------------
;; C Source file format.
;;---------------------------------------------
(defun jcs-c-source-file-format-info ()
  "Header for C source file."
  (jcs-global-file-info)
  )

;;---------------------------------------------
;; C++ Header file format.
;;---------------------------------------------
(defun jcs-c++-header-file-format-info ()
  "Header for C++ header file."

  (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

  (insert "#ifndef __")
  (push-mark)
  (insert BaseFileName)
  (upcase-region (mark) (point))
  (pop-mark)
  (insert "_H__\n")
  (jcs-global-file-info)
  (insert "#define __")
  (push-mark)
  (insert BaseFileName)
  (upcase-region (mark) (point))
  (pop-mark)
  (insert "_H__")
  (insert "\n\n\n")

  ;; >>>> Method 1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ;; Just add the template no matter what.
  ;;(jcs-c++-default-header-template)
  ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ;; OR
  ;; >>>> Method 2 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ;; Ask to add c++ template.
  (call-interactively 'jcs-ask-cpp-default-header)
  ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  (insert "#endif /* __")
  (push-mark)
  (insert BaseFileName)
  (upcase-region (mark) (point))
  (pop-mark)
  (insert "_H__ */\n")
  )

;;---------------------------------------------
;; C++ Source file format.
;;---------------------------------------------
(defun jcs-c++-source-file-format-info ()
  "Header for C++ source file."
  (jcs-global-file-info)

  ;; >>>> Method 1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ;; Just add the template no matter what.
  ;;(jcs-c++-default-source-template)
  ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ;; OR
  ;; >>>> Method 2 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ;; Ask to add c++ template.
  (call-interactively 'jcs-ask-cpp-default-source)
  ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  )

;;---------------------------------------------
;; C++ Default Header and Source Template.
;;---------------------------------------------
(defun jcs-c++-default-header-template ()
  "C++ Default Header Constrcutor and Destructor."
  (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))


  ;; insert class
  (insert "/**\n")
  (insert " * @class ")
  (insert BaseFileName)
  (insert "\n")
  (insert " * @brief Class description...\n")
  (insert " */\n")
  (insert "class ")
  (insert BaseFileName)
  (insert "\n{\n")
  (insert "private:\n\n")
  (insert "public:\n")

  ;; constructor & destructor.
  (insert BaseFileName)
  (insert "();\n")
  (insert "~")
  (insert BaseFileName)
  (insert "();\n\n\n")

  (insert "    /* operator */\n\n")
  (insert "    /* setter */\n\n")
  (insert "    /* getter */\n")

  (insert "\n};")
  (insert "\n\n"))

(defun jcs-c++-default-source-template ()
  "C++ Default Source Constrcutor and Destructor."
  (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

  (insert "\n")
  (insert "#include \"")
  (insert BaseFileName)
  (insert ".h\"\n\n\n")

  ;; insert constructor
  (insert BaseFileName)
  (insert "::")
  (insert BaseFileName)
  (insert "()\n")
  (insert "{\n\n")
  (insert "}\n\n")

  ;; insert destructor
  (insert BaseFileName)
  (insert "::~")
  (insert BaseFileName)
  (insert "()\n")
  (insert "{\n\n")
  (insert "}\n")
  )

;;---------------------------------------------
;; COBOL file header format.
;;---------------------------------------------
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
