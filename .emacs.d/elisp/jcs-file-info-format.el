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
  (insert " *                   Copyright (c) 2016 by Shen, Jen-Chieh $\n")
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
  (insert "   -                   Copyright (c) 2016 by Shen, Jen-Chieh $\n")
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
  (insert "#                     Copyright (c) 2016 by Shen, Jen-Chieh $\n")
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
  (insert ";;                    Copyright (c) 2016 by Shen, Jen-Chieh $\n")
  (insert ";; ========================================================================\n")
  (insert "\n\n")
  )

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-file-info-format.el file
