;;; early-init.el --- Early Init File  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; If Emacs is built with the new MPS-based garbage collector, these variables have no effect
;; Increase gc thresholds during startup
(setq gc-cons-threshold (expt 2 30)
      gc-cons-percentage 0.6
      )

;; Reset gc thresholds
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 800000 4)
                                               gc-cons-percentage 0.2)))

(setq default-frame-alist '((width . 120)
                            (height . 60)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;;; early-init.el ends here
