;;; early-init.el --- Early Init File
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil
      default-frame-alist '((tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (vertical-scroll-bars . nil)))

(set-face-attribute 'default nil
                    :family "Iosevka"
                    :weight 'semi-bold
                    :height 180)
(dolist (attr '(variable-pitch fixed-pitch))
  (set-face-attribute attr nil
                      :family "Iosevka"
                      :weight 'semi-bold
                      :height 1.0))

;;; early-init.el ends here
