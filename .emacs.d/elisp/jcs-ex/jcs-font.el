;; ========================================================================
;; $File: jcs-font.el $
;; $Date: 2018-05-21 17:01:33 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


(defcustom jcs-default-ttf-fonts '(;; >> Classic Console <<
                                   "/.emacs.d/fonts/clacon.ttf"
                                   ;; >> Ubuntu Mono <<
                                   "/.emacs.d/fonts/UbuntuMono-R.ttf")
  "List of TTF fonts you want to use in the currnet OS.")

(defcustom jcs-default-ttf-font-name "Ubuntu Mono"
  "Name of the font we want to use as default.
This you need to check syste manually.")


;;;###autoload
(defun jcs-install-fonts ()
  "Install all .ttf fonts in the `jcs-default-ttf-fonts'."
  (interactive)
  (dolist (default-ttf-font jcs-default-ttf-fonts)
    (let ((font-path default-ttf-font)
          (ttf-file-name (jcs-get-file-name-or-last-dir-fromt-path default-ttf-font t))
          (this-font-install nil))
      ;; NOTE(jenchieh): Start installing to OS.
      (cond (;; Windows
             casey-win32
             (progn
               ;; NOTE(jenchieh): DOS/Windows use `slash' instead of `backslash'.
               (setq font-path (concat (getenv "HOME") default-ttf-font))
               (setq font-path (jcs-replace-string "/" "\\" font-path))

               (when (file-exists-p font-path)
                 ;; Add font file to `Windows/Fonts' directory.
                 (shell-command (concat "echo F|xcopy /y /s /e /o \""
                                        font-path
                                        "\" \"%systemroot%\\Fonts\""))
                 ;; Then add it to the register.
                 (shell-command (concat "reg add \"HKLM\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Fonts\" /v \""
                                        ttf-file-name
                                        " (TrueType)\" /t REG_SZ /d \""
                                        ttf-file-name
                                        "\" /f"))

                 (setq this-font-install t))))
            (;; MacOS
             casey-aquamacs
             (progn
               ;; NOTE(jenchieh): MacOS use `backslash' instead of `slash'.
               (setq font-path (concat (getenv "HOME") default-ttf-font))
               (setq font-path (jcs-replace-string "\\" "/" font-path))

               (when (file-exists-p font-path)
                 (unless (file-directory-p "~/.fonts")
                   (mkdir "~/.fonts" t))

                 ;; TODO(jenchieh): Make it compatible with MacOS.
                 ;; TODO(jenchieh): I try to make it, but I don't have
                 ;; MacOS to test it.
                 (shell-command (concat "cp \"" font-path "\" \"~/Library/Fonts/\""))

                 (setq this-font-install t))
               ))
            (;; Linux Distro
             casey-linux
             (progn
               ;; NOTE(jenchieh): Linux use `backslash' instead of `slash'.
               (setq font-path (concat (getenv "HOME") default-ttf-font))
               (setq font-path (jcs-replace-string "\\" "/" font-path))

               (when (file-exists-p font-path)
                 (unless (file-directory-p "~/.fonts")
                   (mkdir "~/.fonts" t))

                 (shell-command (concat "cp \"" font-path "\" ~/.fonts"))
                 (shell-command "fc-cache -f -v")

                 (setq this-font-install t)))))

      ;; NOTE(jenchieh): Prompt when install the font.
      (if this-font-install
          (message "[Done install font '%s'.]" ttf-file-name)
        (message "[Font '%s' you specify is not install.]" ttf-file-name))
      ))  ;; End 'dolist'.
  (message "[Done install all the fonts.]"))

;;;###autoload
(defun jcs-set-default-font ()
  "Set default font the safe way."
  (interactive)
  ;; NOTE(jenchieh): Install font if not installed.
  (unless (jcs-is-contain-list-string (font-family-list) jcs-default-ttf-font-name)
    (call-interactively #'jcs-install-fonts))

  (if (jcs-is-contain-list-string (font-family-list) jcs-default-ttf-font-name)
      (set-frame-font jcs-default-ttf-font-name nil t)
    (message "[Install fonts process still running, please call `jcs-set-default-font' after a while.]")))
(call-interactively #'jcs-set-default-font)
