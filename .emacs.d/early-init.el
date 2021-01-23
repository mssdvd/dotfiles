(setq package-enable-at-startup nil)

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(dolist (attr '(default variable-pitch))
  (set-face-attribute attr nil :family "Iosevka" :foundry "BE5N"
                      :slant 'normal :weight 'semi-bold :height 181 :width 'normal))
