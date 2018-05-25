;; ========================================================================
;; $File: jcs-font.el $
;; $Date: 2018-05-21 17:01:33 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


(require 'use-ttf)


;; List of TTF fonts you want to use in the currnet OS.
(setq use-ttf-default-ttf-fonts '(;; >> Classic Console <<
                                  "/.emacs.jcs/fonts/clacon.ttf"
                                  ;; >> Ubuntu Mono <<
                                  "/.emacs.jcs/fonts/UbuntuMono-R.ttf"))

;; Name of the font we want to use as default.
;; This you need to check the font name in the system manually.
(setq use-ttf-default-ttf-font-name "Ubuntu Mono")

;; Use the font by `use-ttf-default-ttf-font-name` variable. This will actually
;; set your Emacs to your target font.
(call-interactively #'use-ttf-set-default-font)
