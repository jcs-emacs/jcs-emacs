;;; ui/popup/config.el  -*- lexical-binding: t; -*-

(leaf popup
  :defer-config
  (defvar jcs-popup-mouse-events-flag-p nil
    "Check if `popup-menu-item-of-mouse-event' is called.")

  (defvar jcs-popup-selected-item-flag-p nil
    "Check if `popup-selected-item' is called.")

  (defun jcs-popup-clicked-on-menu-p ()
    "Check if the user actually clicked on the `popup' object."
    (and jcs-popup-mouse-events-flag-p
         (not jcs-popup-selected-item-flag-p)))

  (jcs-advice-add 'popup-menu-item-of-mouse-event :after
    (setq jcs-popup-mouse-events-flag-p t
          jcs-popup-selected-item-flag-p nil))

  (jcs-advice-add 'popup-selected-item :after
    (setq jcs-popup-selected-item-flag-p (jcs-last-input-event-p "mouse-1")))

  (jcs-advice-add 'popup-draw :around
    (if (and (jcs-last-input-event-p "mouse-1")
             (not (jcs-popup-clicked-on-menu-p)))
        (keyboard-quit)
      (apply arg0 args))))

(leaf pos-tip
  :init
  (setq pos-tip-internal-border-width 5))
