;;; app/rss/config.el  -*- lexical-binding: t; -*-

(leaf elfeed
  :hook (elfeed-search-mode-hook . buffer-wrap-mode)
  :init
  (setq elfeed-db-directory (concat user-emacs-directory ".elfeed")
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'delete-window
        elfeed-feeds '(("https://planet.emacslife.com/atom.xml" planet emacslife)
                       ("http://www.masteringemacs.org/feed/" mastering)
                       ("https://oremacs.com/atom.xml" oremacs)
                       ("https://pinecast.com/feed/emacscast" emacscast)
                       ("https://emacstil.com/feed.xml" Emacs TIL))))
