;;; .emacs.el --- Main emacs config file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq pgtk-wait-for-event-timeout nil
      read-process-output-max (* 1024 1024))

(setopt
 backup-directory-alist (list (cons "." (concat user-emacs-directory "backups/")))
 comint-prompt-read-only t
 custom-file (concat user-emacs-directory "custom.el")
 custom-unlispify-tag-names nil
 echo-keystrokes 0.1
 enable-recursive-minibuffers t
 history-delete-duplicates t
 indent-tabs-mode nil
 inhibit-startup-screen t
 kill-whole-line t
 load-prefer-newer t
 minibuffer-follows-selected-frame nil
 minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
 mode-line-compact t
 mouse-autoselect-window t
 next-error-message-highlight t
 read-extended-command-predicate #'command-completion-default-include-p
 require-final-newline t
 ring-bell-function 'ignore
 save-interprogram-paste-before-kill t
 savehist-additional-variables '(compile-command kill-ring register-alist)
 scroll-conservatively 101
 scroll-error-top-bottom t
 scroll-preserve-screen-position t
 set-mark-command-repeat-pop t
 switch-to-buffer-obey-display-actions t
 tab-always-indent 'complete
 tooltip-delay 0.1
 uniquify-buffer-name-style 'forward
 use-package-always-defer t
 use-package-enable-imenu-support t
 use-package-hook-name-suffix nil
 use-short-answers t
 user-mail-address "dm@mssdvd.com"
 view-read-only t
 window-resize-pixelwise t
 )

(auth-source-pass-enable)
(blink-cursor-mode 0)
(column-number-mode 1)
(electric-pair-mode 1)
(find-function-mode 1)
(minibuffer-depth-indicate-mode 1)
(savehist-mode 1)
(size-indication-mode 1)
(temp-buffer-resize-mode 1)

(dolist (hook '(conf-mode-hook prog-mode-hook text-mode-hook))
  (add-hook hook (lambda () (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(keymap-global-set "<remap> <capitalize-word>" #'capitalize-dwim)
(keymap-global-set "<remap> <count-words-region>" #'count-words)
(keymap-global-set "<remap> <downcase-word>" #'downcase-dwim)
(keymap-global-set "<remap> <upcase-word>" #'upcase-dwim)
(keymap-global-set "<remap> <zap-to-char>" #'zap-up-to-char)
(keymap-global-set "M-]" #'mode-line-other-buffer)
(keymap-global-set "M-o" #'other-window)

(keymap-global-unset "C-z")
(keymap-global-unset "C-x C-z")

(dolist (fn '(dired-find-alternate-file
              erase-buffer
              list-timers
              narrow-to-region
              scroll-left
              set-goal-column))
  (put fn 'disabled nil))

(load custom-file 'noerror 'nomessage)

;;;;
;; Custom functions (prefixed with +)
;;;;

(defun +check-emacs-updates ()
  "Update Emacs master and packages."
  (interactive)
  (list-packages)
  (magit-status-setup-buffer "~/src/emacs")
  (magit-git-fetch "origin" nil))

(defun +yank-primary ()
  "Insert the primary selection at the position."
  (interactive)
  (let ((primary (gui-get-primary-selection)))
    (push-mark)
    (insert-for-yank primary)))
(keymap-global-set "S-<insert>" #'+yank-primary)

(defun +pass-get-keep-asking (entry)
  "Return ENTRY secret or keep asking until the provided password is correct."
  (let ((pass))
    (while (not (setq pass (auth-source-pass-get 'secret entry))))
    pass))

(defun +replace-unicode-code-points ()
  "Replace Unicode code points with their respective glyph."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx "U+" (group (repeat 4 6 hex-digit))) nil t)
      (replace-match (string (string-to-number (match-string 1) 16))))))

(defun +google-search (term)
  "Search Google for TERM."
  (interactive "sSearch term: ")
  (let ((browse-url-browser-function
         (default-value 'browse-url-browser-function)))
    (browse-url (format "https://google.com/search?q=%s" term))))

(defun +google-flymake (point)
  "Search Google for Flymake diagnostics at POINT."
  (interactive "d")
  (if-let ((diags (flymake-diagnostics point)))
      (dolist (txt diags)
        (+google-search (flymake-diagnostic-text txt)))
    (user-error "No Flymake diagnostics at point")))

(defun +display-system-status-when-fullscreen (frame)
  "Enable `display-time-mode' and `display-battery-mode' if FRAME is fullscreen."
  (if (eq 'fullboth (frame-parameter frame 'fullscreen))
      (progn
        (display-time-mode 1)
        (require 'battery)
        (when (equal (alist-get ?L (funcall battery-status-function))
                     "off-line")
          (display-battery-mode 1)))
    (when (bound-and-true-p display-time-mode)
      (display-time-mode 0))
    (when (bound-and-true-p display-battery-mode)
      (display-battery-mode 0))))
(add-hook 'window-size-change-functions
          #'+display-system-status-when-fullscreen)

;;;;
;; use-package
;;;;

(use-package package
  :custom (package-archive-priorities '(("gnu" . 1)
                                        ("nongnu" . 1)))
  :config
  (unless emacs-repository-branch
    (setopt package-install-upgrade-built-in t))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package delight
  :ensure)

(use-package modus-themes
  :ensure
  :demand
  :bind ("C-c q" . modus-themes-toggle)
  :custom
  (modus-themes-common-palette-overrides
   '((bg-mode-line-active bg-cyan-intense)
     (bg-region bg-ochre)
     (fg-region unspecified)))
  (modus-themes-italic-constructs t)
  :config
  (if (ignore-errors
        (equal (process-lines
                "gsettings" "get"
                "org.gnome.desktop.interface" "color-scheme")
               '("'prefer-dark'")))
      (modus-themes-load-theme 'modus-vivendi)
    (modus-themes-load-theme 'modus-operandi)))

(use-package time
  :custom
  (display-time-24hr-format t)
  (display-time-load-average-threshold 0.5))

(use-package display-line-numbers
  :custom (display-line-numbers-grow-only t))

(use-package repeat
  :demand
  :custom (repeat-exit-key "RET")
  :config (repeat-mode 1))

(use-package winner
  :demand
  :custom (winner-boring-buffers-regexp " *Minibuf-[0-9]+")
  :config (winner-mode 1))

(use-package windmove
  :bind-keymap ("C-c w" . +windmove-repeat-map)
  :bind
  (:repeat-map +windmove-repeat-map
               ("<left>" . windmove-left)
               ("<right>" . windmove-right)
               ("<up>" . windmove-up)
               ("<down>" . windmove-down)
               ("S-<left>" . windmove-swap-states-left)
               ("S-<right>" . windmove-swap-states-right)
               ("S-<up>" . windmove-swap-states-up)
               ("S-<down>" . windmove-swap-states-down))
  :custom (windmove-wrap-around t))

(use-package diff-mode
  :custom (diff-font-lock-prettify t))

(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer)
  (:map ibuffer-mode-map
        ("M-o" . other-window)))

(use-package dired
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-information-lines nil)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alhv --group-directories-first")
  :hook (dired-mode-hook . (lambda ()
                             (setq-local truncate-lines t)
                             (dired-hide-details-mode 1))))

(use-package dired-aux
  :custom
  (dired-vc-rename-file t)
  (shell-command-guess-functions '(shell-command-guess-open shell-command-guess-xdg)))

(use-package wdired
  :custom (wdired-allow-to-change-permissions t))

(use-package vertico
  :ensure
  :demand
  :bind
  ("M-R" . vertico-repeat)
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)
        ("C-<backspace>" . vertico-directory-delete-word))
  :custom
  (vertico-cycle t)
  (vertico-multiform-categories '((consult-grep buffer)
                                  (file grid)
                                  (imenu buffer
                                         (vertico-buffer-display-action
                                          . (display-buffer-in-direction
                                             (direction . right)
                                             (window-width . 0.3))))))
  (vertico-multiform-commands '((consult-line buffer)
                                (consult-line-multi buffer)
                                (consult-outline buffer)
                                (xref-find-apropos buffer)
                                (xref-find-references buffer)))
  (vertico-scroll-margin (/ vertico-count 2))
  :config
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  (vertico-multiform-mode 1)
  :hook
  (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  (minibuffer-setup-hook . vertico-repeat-save))

(use-package orderless
  :ensure
  :demand
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-ignore-case t)
  (orderless-matching-styles '(orderless-initialism
                               orderless-literal
                               orderless-prefixes
                               orderless-regexp))
  (read-file-name-completion-ignore-case t))

(use-package consult
  :ensure
  :bind
  ("C-c h" . consult-history)
  ("C-c i" . consult-info)
  ("C-c x" . consult-mode-command)
  ("M-g I" . consult-imenu-multi)
  ("M-g e" . consult-compile-error)
  ("M-g f" . consult-flymake)
  ("M-g k" . consult-global-mark)
  ("M-g m" . consult-mark)
  ("M-g o" . consult-outline)
  ("M-s D" . consult-locate)
  ("M-s L" . consult-line-multi)
  ("M-s d" . consult-find)
  ("M-s g" . +consult-smart-grep)
  ("M-s k" . consult-keep-lines)
  ("M-s l" . consult-line)
  ("M-s u" . consult-focus-lines)
  ([remap bookmark-jump] . consult-bookmark)
  ([remap copy-to-register] . consult-register)
  ([remap imenu] . consult-imenu)
  ([remap project-switch-to-buffer] . consult-project-buffer)
  ([remap repeat-complex-command] . consult-complex-command)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap yank-pop] . consult-yank-pop)
  (:map isearch-mode-map
        ("M-s L" . consult-line-multi)
        ("M-s l" . consult-line)
        ([remap isearch-edit-string] . consult-isearch-history))
  (:map minibuffer-local-map
        ([remap next-matching-history-element] . consult-history)
        ([remap previous-matching-history-element] . consult-history))
  :config
  (defun +consult-smart-grep (&optional dir initial)
    (interactive "P")
    (if (eq (nth 1 (project-current)) 'Git)
        (consult-git-grep dir initial)
      (consult-ripgrep dir initial)))

  (defvar-keymap +consult-line-map
    "C-s" #'previous-history-element)

  (consult-customize consult-line :keymap +consult-line-map)
  (add-to-list 'consult-preview-excluded-files "\\.pdf\\'")
  :custom
  (consult-narrow-key "<")
  (register-preview-function #'consult-register-format)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref))

(use-package marginalia
  :ensure
  :demand
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :config (marginalia-mode 1))

(use-package embark
  :ensure
  :bind
  ("C-." . embark-act)
  ("C-h B" . embark-bindings)
  (:map embark-flymake-map
        ("F" . +google-flymake))
  (:map embark-general-map
        ("G" . +google-search))
  (:map minibuffer-local-map
        ("C-," . embark-export))
  :custom (embark-quit-after-action '((embark-recentf-remove . nil)
                                      (kill-buffer . nil)
                                      (t . t))))

(use-package embark-consult
  :ensure
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package corfu
  :ensure
  :demand
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  :config (global-corfu-mode 1))

(use-package corfu-history
  :demand
  :after corfu
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (corfu-history-mode 1))

(use-package corfu-popupinfo
  :demand
  :after corfu
  :custom (corfu-popupinfo-delay '(2.0 . 0.5))
  :config (corfu-popupinfo-mode 1))

(use-package cape
  :ensure
  :demand
  :bind ("C-c p" . cape-dict)
  :custom
  (cape-dict-file (if-let ((file (expand-file-name "~/.words_us-it"))
                           ((file-readable-p file)))
                      file
                    "/usr/share/dict/words"))
  :config (add-to-list 'completion-at-point-functions #'cape-file))

(use-package isearch
  :bind
  (:map isearch-mode-map
        ("M-c" . nil)
        ("C-," . isearch-occur))
  :custom
  (isearch-allow-motion t)
  (isearch-allow-scroll 'unlimited)
  (isearch-lazy-count t)
  (isearch-wrap-pause 'no)
  (search-default-mode #'char-fold-to-regexp)
  (search-whitespace-regexp ".*?"))

(use-package grep
  :custom (grep-use-headings t))

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("C-c f n" . flymake-goto-next-error)
        ("C-c f p" . flymake-goto-prev-error)
        ("C-c f f" . flymake-show-buffer-diagnostics)
        ("C-c f F" . flymake-show-project-diagnostics))
  (:map project-prefix-map
        ("!" . flymake-show-project-diagnostics))
  :custom
  (flymake-indicator-type 'fringes)
  (flymake-mode-line-lighter "FM")
  (flymake-show-diagnostics-at-end-of-line t)
  :config
  (defvar-keymap +flymake-repeat-map
    :doc "Keymap to repeat `flymake-goto-next-error' and
`flymake-goto-prev-error'.  Used in `repeat-mode'"
    :repeat t
    "n" #'flymake-goto-next-error
    "p" #'flymake-goto-prev-error)
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
  :hook prog-mode-hook)

(use-package compile
  :bind ([f5] . recompile)
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error))

(use-package ansi-color
  :hook (compilation-filter-hook . ansi-color-compilation-filter))

(use-package ansi-osc
  :custom (ansi-osc-for-compilation-buffer t)
  :hook (compilation-filter-hook . ansi-osc-compilation-filter))

(use-package recentf
  :demand
  :custom (recentf-max-saved-items 500)
  :config (recentf-mode 1))

(use-package saveplace
  :defer 1
  :custom
  (save-place-autosave-interval (* 60 5))
  (save-place-limit 1600)
  :config (save-place-mode 1))

(use-package desktop
  :custom
  (desktop-globals-to-clear nil)
  (desktop-globals-to-save '(desktop-missing-file-warning)))

(use-package rainbow-mode
  :ensure
  :delight
  :hook (conf-mode-hook css-mode-hook sgml-mode-hook))

(use-package cdlatex
  :ensure
  :custom (cdlatex-math-modify-alist '((?B "\\mathbb" nil t nil nil))))

(use-package tex
  :ensure auctex)

(use-package org
  :bind
  ("C-c l" . org-store-link)
  (:map org-mode-map
        ([f6] . org-latex-preview)
        ([f8] . insert-char))
  :custom
  (org-M-RET-may-split-line '((default . nil)))
  (org-attach-auto-tag nil)
  (org-babel-load-languages '((emacs-lisp . t)
                              (gnuplot . t)
                              (python . t)
                              (shell . t)))
  (org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %TIMESTAMP %SCHEDULED %DEADLINE")
  (org-confirm-babel-evaluate nil)
  (org-ctrl-k-protect-subtree t)
  (org-cycle-separator-lines 1)
  (org-edit-src-content-indentation 0)
  (org-ellipsis " ▶")
  (org-enforce-todo-checkbox-dependencies t)
  (org-enforce-todo-dependencies t)
  (org-file-apps '((auto-mode . emacs)
                   (directory . emacs)
                   ("\\.mm\\'" . default)
                   ("\\.x?html?\\'" . default)
                   ("\\.pdf\\'" . browse-url)
                   ("\\.webm\\'" . "mpv %s")
                   ("\\.odt\\'" . "libreoffice %s")))
  (org-highlight-latex-and-related '(latex entities))
  (org-html-validation-link nil)
  (org-latex-packages-alist '(("" "color") ("" "listings") ("AUTO" "babel" t ("pdflatex"))))
  (org-log-into-drawer t)
  (org-outline-path-complete-in-steps nil)
  (org-pretty-entities t)
  (org-return-follows-link t)
  (org-startup-folded t)
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WIP(w)" "REDO(r)" "|" "DONE(d)" "CANCELED(c)")))
  (org-track-ordered-property-with-tag t)
  (org-use-fast-tag-selection t)
  (org-use-speed-commands t)
  :hook
  (org-mode-hook . visual-line-mode)
  (org-mode-hook . turn-on-org-cdlatex)
  (org-mode-hook . (lambda () (display-line-numbers-mode 0))))

(use-package org-agenda
  :bind ("C-c a" . org-agenda)
  :custom (org-agenda-sticky t))

(use-package org-capture
  :bind ("C-c c" . org-capture)
  :custom
  (org-capture-templates
   `(("m" "Next week menu" entry
      (file+headline "~/notes/meals.org"
                     ,(format-time-string "%Y"))
      "* W%^{Week number|%(number-to-string (1+ (string-to-number (format-time-string \"%V\"))))|%(format-time-string \"%V\")}

|---+-----+-----+-----+-----+-----+-----+-----|
|   | Mon | Tue | Wed | Thu | Fri | Sat | Sun |
|---+-----+-----+-----+-----+-----+-----+-----|
| L |     |     |     |     |     |     |     |
| D |     |     |     |     |     |     |     |
|---+-----+-----+-----+-----+-----+-----+-----|"
      :empty-lines-before 1
      :jump-to-captured t)

     ("s" "New logged org-pomodoro" entry
      (file+olp+datetree "~/notes/activities.org"
                         "Log")
      "* %?"
      :empty-lines 0
      :tree-type week
      :before-finalize (org-pomodoro))

     ("S" "New activity log (clock in)" entry
      (file+olp+datetree "~/notes/activities.org"
                         "Log")
      "* %?"
      :clock-in it
      :clock-keep t
      :jump-to-captured t
      :empty-lines 0
      :tree-type week)

     ("w" "Weight" table-line
      (file "~/notes/weight.org")
      "| %u | %? |"
      :kill-buffer t))))

(use-package org-indent
  :delight
  :custom
  (org-indent-indentation-per-level 1)
  (org-indent-mode-turns-on-hiding-stars nil)
  :hook org-mode-hook)

(use-package gnuplot
  :ensure)

(use-package org-pomodoro
  :ensure
  :bind ("C-c r" . org-pomodoro)
  :custom
  (org-pomodoro-expiry-time 40)
  (org-pomodoro-keep-killed-pomodoro-time t)
  (org-pomodoro-audio-player (concat (executable-find "mpv") " --volume=75"))
  (org-pomodoro-manual-break t))

(use-package markdown-mode
  :ensure
  :hook (markdown-mode-hook . visual-line-mode))

(use-package alert
  :ensure
  :custom (alert-default-style 'libnotify))

(use-package org-caldav
  :disabled
  :config
  (setq org-caldav-url "https://cdav.migadu.com/calendars/dm@mssdvd.com"
        org-caldav-inbox "~/org/calendar.org"
        org-caldav-calendar-id "home"
        org-icalendar-timezone "Europe/Rome"))

(use-package org-download
  :ensure
  :functions (org-redisplay-inline-images)
  :after org
  :config
  (setq  org-download-screenshot-method "grim -g \"$(slurp)\" %s"
         org-download-image-dir "./org_download")
  ;; Needed because new images are not indented
  (advice-add 'org-download-screenshot :after (lambda () (org-redisplay-inline-images))))

(use-package denote
  :ensure
  :bind
  ("C-c n n" . denote)
  ("C-c n f" . +denote-find-file)
  ("C-c n i" . denote-link)
  ("C-c n I" . denote-link-add-links)
  ("C-c n l" . denote-link-find-file)
  ("C-c n b" . denote-link-backlinks)
  ("C-c n r" . denote-rename-file)
  :custom
  (denote-backlinks-show-context t)
  (denote-directory (expand-file-name "~/denote/"))
  (denote-dired-directories (list denote-directory))
  (denote-known-keywords nil)
  :config
  (defun +denote-find-file ()
    (interactive)
    (let ((default-directory denote-directory)
          (completion-ignored-extensions (cons ".git/" completion-ignored-extensions)))
      (call-interactively #'find-file)))
  :hook
  (denote-backlinks-mode-hook . (lambda () (setq-local truncate-lines t)))
  (dired-mode-hook . denote-dired-mode-in-directories))

(use-package vc
  :custom (vc-follow-symlinks t))

(use-package magit
  :ensure
  :bind
  (:map project-prefix-map
        ("m" . magit-project-status))
  :custom
  (magit-delete-by-moving-to-trash nil)
  (magit-diff-refine-hunk 'all)
  (magit-save-repository-buffers 'dontask)
  (magit-status-goto-file-position t)
  :config (put 'magit-edit-line-commit 'disabled nil))

(use-package forge
  :ensure)

(use-package with-editor
  :hook ((shell-mode-hook
          eshell-mode-hook
          term-exec-hook)
         . with-editor-export-editor))

(use-package epg
  :custom (epg-pinentry-mode 'loopback))

(use-package password-store
  :ensure)

(use-package git-modes
  :ensure)

(use-package diff-hl
  :ensure
  :custom (diff-hl-draw-borders nil)
  :hook (dired-mode-hook . diff-hl-dired-mode-unless-remote))

(use-package tempel
  :ensure
  :bind (("M-+" . tempel-insert))
  :custom (tempel-trigger-prefix "<")
  :hook
  ((conf-mode-hook eglot-managed-mode-hook prog-mode-hook text-mode-hook) .
   (lambda ()
     (setq-local completion-at-point-functions
                 (cons #'tempel-expand
                       completion-at-point-functions)))))


(use-package autorevert
  :delight auto-revert-mode
  :defer 1
  :custom (auto-revert-avoid-polling t)
  :config (global-auto-revert-mode 1))

(use-package eldoc
  :delight
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose)
  (eldoc-echo-area-prefer-doc-buffer t))

(use-package eshell
  :bind ("C-c e" . eshell)
  :config
  (setopt eshell-modules-list
          (seq-union '(eshell-elecslash
                       eshell-smart
                       eshell-tramp)
                     eshell-modules-list)))

(use-package ansi-color
  :hook (compilation-filter-hook . ansi-color-compilation-filter))
(use-package esh-mode
  :bind
  (:map eshell-mode-map
        ([remap eshell-previous-matching-input] . cape-history)))

(use-package em-ls
  :custom (eshell-ls-initial-args "-h"))

(use-package em-term
  :custom
  (eshell-visual-options '(("git" "--help" "--paginate")))
  (eshell-visual-subcommands '(("git" "log" "l")))
  :config
  (setopt eshell-visual-commands
          (seq-union '("gh" "watch") eshell-visual-commands)))


(use-package shell
  :custom (shell-has-auto-cd t))

(use-package sh-script
  :custom (sh-shell-file "/bin/sh")
  :hook
  (sh-base-mode-hook .
                     (lambda ()
                       (when (equal (file-name-nondirectory buffer-file-name)
                                    "PKGBUILD")
                         (setq-local flymake-show-diagnostics-at-end-of-line nil)))))

(use-package nov
  :ensure
  :mode ("\\.epub\\'" . nov-mode))

(use-package terminal-here
  :ensure
  :bind ("C-c t" . terminal-here-launch)
  :custom (terminal-here-terminal-command 'foot))

(use-package jinx
  :ensure
  :bind ([remap ispell-word] . jinx-correct)
  :custom (jinx-languages "en_US it_IT"))

(use-package apropos
  :custom (apropos-do-all t))

(use-package fish-mode
  :ensure)

(use-package ediff
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

(use-package gdb-mi
  :custom (gdb-many-windows t))

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :custom
  (hippie-expand-try-functions-list
   '(try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list try-expand-line
     try-expand-line-all-buffers
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol)))

(use-package abbrev
  :delight
  :custom (save-abbrevs 'silently)
  :hook text-mode-hook)

(use-package man
  :bind ("C-c k" . man)
  :custom (Man-notify-method 'aggressive))

(use-package wolfram
  :ensure
  :custom
  (wolfram-alpha-app-id (+pass-get-keep-asking "wolfram_alpha_app_id"))
  (wolfram-alpha-magnification-factor 1.5))

(use-package systemd
  :ensure
  :mode
  ("\\.service\\'" . systemd-mode)
  ("\\.timer\\'" . systemd-mode))

(use-package calc
  :config (setq calc-group-digits t)
  :custom
  (calc-kill-line-numbering nil)
  (calc-make-windows-dedicated t)
  (calc-multiplication-has-precedence nil))

(use-package calendar
  :custom (calendar-week-start-day 1))

(use-package help-fns
  :custom (help-enable-variable-value-editing t)
  :config (put 'help-fns-edit-variable 'disabled nil)
  :hook (help-fns-describe-function-functions . shortdoc-help-fns-examples-function))

(use-package ledger-mode
  :ensure
  :mode ("\\.ldg\\'" . ledger-mode)
  :bind
  (:map ledger-mode-map
        ([f6] . (lambda ()
                  (interactive)
                  (insert (if (eq (char-before) 32)
                              "EUR"
                            " EUR"))
                  (ledger-post-align-dwim))))
  :custom
  (ledger-copy-transaction-insert-blank-line-after t)
  (ledger-default-date-format "%Y-%m-%d")
  (ledger-highlight-xact-under-point nil)
  (ledger-reconcile-default-commodity " EUR")
  :hook (ledger-mode-hook . (lambda ()
                              (setq-local corfu-auto nil))))

(use-package ledger-flymake
  :hook (ledger-mode-hook . ledger-flymake-enable))

(use-package csv-mode
  :ensure
  :custom (csv-separators '("," ";" "	"))
  :hook (csv-mode-hook . csv-guess-set-separator))

(use-package eat
  :ensure
  :bind ("C-c v" . eat)
  :custom (eat-kill-buffer-on-exit t)
  :hook (eshell-load-hook . eat-eshell-visual-command-mode))

(use-package shr
  :custom
  (shr-use-colors nil)
  (shr-use-fonts nil))

(use-package yaml-mode
  :ensure)

(use-package matlab-mode
  :ensure
  :mode ("\\.m\\'" . matlab-mode)
  :custom (matlab-shell-command-switches '("-nodesktop" "-nosplash")))

  :custom
  :config

(use-package re-builder
  :custom (reb-re-syntax 'string))

(use-package mouse
  :custom (mouse-yank-at-point t)
  :config (context-menu-mode 1))

(use-package bookmark
  :custom (bookmark-save-flag 1))

(use-package paren
  :custom (show-paren-context-when-offscreen 'child-frame))

(use-package eww
  :custom
  (eww-auto-rename-buffer 'title)
  (eww-search-prefix "https://lite.duckduckgo.com/lite/?q="))

(use-package project
  :custom
  (project-kill-buffers-display-buffer-list t)
  (project-switch-use-entire-map t))

(use-package tab-bar
  :custom (tab-bar-show 1))

(use-package tmm
  :config (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions))

(use-package olivetti
  :ensure)

(use-package osm
  :ensure
  :custom
  (osm-copyright nil)
  (osm-home '(41.9 12.5 6))
  (osm-search-language "en,it"))

(use-package hexl
  :hook (hexl-mode-hook . read-only-mode))

(use-package vundo
  :ensure
  :bind ("C-c u" . vundo)
  :custom
  (vundo-compact-display t)
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package outline
  :hook ((apropos-mode-hook xref-after-update-hook) . outline-minor-mode))

(use-package tmr
  :ensure)

(use-package subword
  :hook (go-mode-hook go-ts-mode-hook js-base-mode-hook))

(use-package proced
  :custom
  (proced-enable-color-flag t)
  (proced-format 'medium))

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll" :rev :newest)
  :demand
  :config (ultra-scroll-mode 1))

;;
;; Mail
;;

(use-package message
  :custom
  (message-auto-save-directory nil)
  (message-confirm-send t)
  (message-kill-buffer-on-exit t)
  (message-sendmail-envelope-from 'header))

(use-package sendmail
  :custom
  (mail-specify-envelope-from t)
  (send-mail-function #'sendmail-send-it)
  (sendmail-program "/usr/bin/msmtp"))

(use-package notmuch
  :commands (notmuch notmuch-search +sync-email)
  :bind
  ("C-x m" . notmuch-mua-new-mail)
  ("C-c m" . (lambda ()
               (interactive)
               (if (equal
                    (shell-command-to-string
                     "notmuch count tag:unread")
                    "0\n")
                   (notmuch)
                 (notmuch-search "tag:unread" t))))
  :custom
  (mail-user-agent 'notmuch-user-agent)
  (notmuch-draft-folder "dm@mssdvd.com/Drafts")
  (notmuch-fcc-dirs
   '(("dm@mssdvd.com" . "dm@mssdvd.com/Sent +mssdvd +sent")
     ("d.masserut@gmail.com" . "\"d.masserut@gmail.com/[Gmail]/Sent Mail\" +gmail +sent")))
  (notmuch-hello-recent-searches-max 15)
  (notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread inbox" :query "tag:unread and tag:inbox"
            :sort-order oldest-first :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "last week" :query "date:\"7D\".." :key "w")
     (:name "last 3 months" :query "date:\"3M\".." :key "m")
     (:name "all mail" :query "*" :key "a")))
  (notmuch-search-oldest-first nil)
  (notmuch-search-result-format '(("date" . "%12s ")
                                  ("count" . "%-7s ")
                                  ("authors" . "%-30s ")
                                  ("subject" . "%s ")
                                  ("tags" . "(%s)")))
  (notmuch-show-all-tags-list t)
  (notmuch-show-part-button-default-action #'notmuch-show-interactively-view-part)
  :config
  (defun +sync-email ()
    "Sync emails and update notmuch index."
    (interactive)
    (start-process "sync emails and update notmuch index" nil
                   "systemctl" "--user" "start" "sync_email.service"))
  :hook
  (notmuch-after-tag-hook
   . (lambda ()
       (when (equal (getenv "XDG_CURRENT_DESKTOP") "sway")
         (start-process "update mail indicator" nil
                        "pkill" "-SIGRTMIN+1" "waybar")))))

(use-package notmuch-indicator
  :ensure
  :demand
  :config (notmuch-indicator-mode 1))

;;
;; Languages configurations
;;

(use-package eglot
  :ensure
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  :config
  (setq-default eglot-workspace-configuration
                '((:gopls . ((gofumpt . t)
                             (linkTarget . "godocs.io")
                             (staticcheck . t)
                             (usePlaceholders . t)))))
  :hook
  ((c-mode-hook
    c++-mode-hook
    go-mode-hook
    go-dot-mod-mode-hook
    go-ts-mode-hook
    go-mod-ts-mode-hook
    rust-mode-hook
    rust-ts-mode-hook)
   . (lambda ()
       (setq-local flymake-show-diagnostics-at-end-of-line nil)
       (eglot-ensure)))
  ((go-mode-hook
    go-dot-mod-mode-hook
    go-ts-mode-hook
    go-mod-ts-mode-hook
    rust-mode-hook
    rust-ts-mode-hook)
   . (lambda ()
       (add-hook 'before-save-hook #'eglot-format nil t))))

(use-package go-mode
  :ensure)


(use-package cc-vars
  :custom
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (other . "linux"))))

(use-package python
  :custom
  (python-flymake-command
   '("ruff" "check" "--quiet" "--stdin-filename=stdin" "-"))
  (python-flymake-msg-alist
   '(("\(^redefinition\|.*unused.*\|used$\)" . :warning)
     ("^E999" . :error)
     ("^[EW][0-9]+" . :note))))

(use-package lua-mode
  :ensure)

(use-package rust-mode
  :ensure)
(use-package js
  :hook (js-mode-hook . (lambda () (setq-local indent-tabs-mode nil))))

;;; .emacs.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
