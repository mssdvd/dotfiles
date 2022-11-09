;;; early-init.el --- Early Init File  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq custom-file (concat user-emacs-directory "etc/custom.el")
      ;; Increase gc thresholds during startup
      gc-cons-threshold most-positive-fixnum
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
              ("T480s" 181)
              (_ 98))))
  (set-face-attribute 'default nil
                      :family "Iosevka Fixed"
                      :height size))

(dolist (attr '(variable-pitch fixed-pitch))
  (set-face-attribute attr nil
                      :family (face-attribute 'default :family)))

;;; early-init.el ends here
