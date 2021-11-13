;;; early-init.el --- Early Init File  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq gc-cons-threshold #x40000000)

(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(let ((size (pcase (system-name)
              ("T480s" 181)
              (_ 98))))
  (set-face-attribute 'default nil
                      :family "Iosevka Fixed"
                      :height size))

(dolist (attr '(variable-pitch fixed-pitch))
  (set-face-attribute attr nil
                      :family "Iosevka Fixed"
                      :height 1.0))

;;; early-init.el ends here
