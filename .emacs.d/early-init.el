;;; early-init.el --- Early Init File  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil
      default-frame-alist '((tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (vertical-scroll-bars . nil)))
(let ((size (pcase (system-name)
              ("T480s" 181)
              (_ 98))))
  (set-face-attribute 'default nil
                      :family "Iosevka Fixed"
                      :weight 'semi-bold
                      :height size))

(dolist (attr '(variable-pitch fixed-pitch))
  (set-face-attribute attr nil
                      :family "Iosevka Fixed"
                      :weight 'semi-bold
                      :height 1.0))

;;; early-init.el ends here
