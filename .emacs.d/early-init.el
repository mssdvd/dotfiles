;;; early-init.el --- Early Init File  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Increase gc thresholds during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reset gc thresholds
(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold 1600000
                                      gc-cons-percentage 0.1)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(let ((size (pcase (system-name)
              ("T480s" 110)
              (_ 100))))
  (set-face-attribute 'default nil :height size))

;;; early-init.el ends here
