;;; .emacs.el --- Main emacs config file  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(setq read-process-output-max (* 1024 1024 4)) ;; 4mb

(setopt use-package-always-defer t
        use-package-enable-imenu-support t)

;; disable cursor blinking
(blink-cursor-mode 0)

;; disable startup screen
(setq inhibit-startup-screen t)

;; change all prompts to y or n
(setq use-short-answers t)

;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; use tab to indent and complete
(setq tab-always-indent 'complete)

(setq echo-keystrokes 0.1)

;; remove suspend-frame binding
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; replace zap-to-char with zap-up-to-char
(keymap-global-set "M-z" #'zap-up-to-char)

;; ibuffer is better
(keymap-global-set "C-x C-b" #'ibuffer)

;; switch to previous buffer
(keymap-global-set "M-]" #'mode-line-other-buffer)

;; uniquify
(setq uniquify-buffer-name-style 'forward)

;; Prefer newer files
(setq load-prefer-newer t)

;; Disable bell
(setq ring-bell-function 'ignore)

;; Make mode-line more compact
(setq mode-line-compact t)

;; Show trailing whitespaces
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace t))))


;;;;
;; Custom functions (prefixed with +)
;;;;

(defun +check-emacs-updates ()
  "Update Emacs master and packages."
  (interactive)
  (list-packages)
  (magit-status "~/src/emacs-mssdvd-git/emacs-master/")
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

;;;;
;; use-package
;;;;

(use-package package
  :config (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; delight
;; https://savannah.nongnu.org/projects/delight
(use-package delight
  :ensure)

(use-package modus-themes
  :ensure
  :pin gnu
  :demand
  :bind ("C-c q" . modus-themes-toggle)
  :custom
  (modus-themes-common-palette-overrides
   '((bg-region bg-lavender)
     (fg-region unspecified)))
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-italic-constructs t)
  :config
  (if (string=
       (shell-command-to-string
        "gsettings get org.gnome.desktop.interface gtk-theme")
       "'Adwaita'\n")
      (load-theme 'modus-operandi :no-confirm)
    (load-theme 'modus-vivendi :no-confirm)))

(use-package cus-edit
  :custom
  (custom-file (expand-file-name "custom.el"  user-emacs-directory))
  (custom-unlispify-tag-names nil)
  :config (load custom-file 'noerror 'nomessage))

(use-package time
  :custom (display-time-24hr-format t))

(use-package simple
  :bind
  ([remap count-words-region] . count-words)
  ([remap upcase-word] . upcase-dwim)
  ([remap downcase-word] . downcase-dwim)
  ([remap capitalize-word] . capitalize-dwim)
  :custom
  (next-error-message-highlight t)
  (save-interprogram-paste-before-kill t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :config
  (column-number-mode 1)
  (size-indication-mode 1))

(use-package window
  :bind ("M-o" . other-window)
  :custom
  (scroll-preserve-screen-position t)
  (switch-to-buffer-obey-display-actions t)
  (display-buffer-alist
   '(("\\*Install vterm\\*" display-buffer-no-window (allow-no-window . t))))
  (window-resize-pixelwise t))

(use-package elec-pair
  :defer 1
  :config (electric-pair-mode 1))

;; display-line-numbers
(use-package display-line-numbers
  :custom (display-line-numbers-grow-only t)
  :hook (conf-mode nxml-mode prog-mode yaml-mode))

;; display-fill-column-indicator
(use-package display-fill-column-indicator
  :hook (conf-mode markdown-mode prog-mode))

;; diff
(use-package diff-mode
  :custom (diff-font-lock-prettify t))

;; dired
(use-package dired
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-information-lines nil)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alhv --group-directories-first")
  :config (put 'dired-find-alternate-file 'disabled nil)
  :hook (dired-mode . dired-hide-details-mode))

(use-package dired-aux
  :custom (dired-vc-rename-file t))

(use-package wdired
  :custom (wdired-allow-to-change-permissions t))

(use-package minibuffer
  :custom
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
  (enable-recursive-minibuffers t)
  :config
  (minibuffer-depth-indicate-mode 1)
  :hook (minibuffer-setup-hook . cursor-intangible-mode))

(use-package vertico
  :ensure
  :demand
  :commands (vertico-mode)
  :functions (vertico-mouse-mode consult-completion-in-region)
  :defines (vertico-quick1)
  :bind
  ("C-c i" . vertico-repeat)
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)
        ("C-<backspace>" . vertico-directory-delete-word)
        ("C-;" . vertico-quick-insert)
        ("C-'" . vertico-quick-exit))
  :custom
  (vertico-cycle t)
  (vertico-quick1 "asdfghjkl;")
  (vertico-scroll-margin (/ vertico-count 2))
  (completion-in-region-function
   (lambda (&rest args)
     (apply (if vertico-mode
                #'consult-completion-in-region
              #'completion--in-region)
            args)))
  :config
  (vertico-mode)
  (vertico-mouse-mode 1)
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  (minibuffer-setup . vertico-repeat-save))

(use-package orderless
  :ensure
  :demand
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-ignore-case t)
  (completion-ignored-extensions (remove ".git/" completion-ignored-extensions))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-literal
                               orderless-regexp
                               orderless-initialism))
  (read-file-name-completion-ignore-case t))

(use-package savehist
  :init (savehist-mode))

(use-package consult
  :ensure
  :pin gnu
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c x" . consult-mode-command)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (defvar +consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      map))
  (consult-customize consult-line :keymap +consult-line-map)
  :custom
  (consult-narrow-key "<")
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format))

(use-package marginalia
  :pin gnu
  :ensure
  :demand
  :commands marginalia-mode
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :config (marginalia-mode))

(use-package embark
  :pin gnu
  :ensure
  :defer 1
  :bind
  ("C-." . embark-act)
  ("C-h B" . embark-bindings)
  (:map embark-general-map
        ("G" . +embark-google-search))
  :config
  (defun +embark-google-search (term)
    "Search Google for TERM."
    (interactive "sSearch Term: ")
    (browse-url
     (format "https://google.com/search?q=%s" term))))

(use-package embark-consult
  :pin gnu
  :ensure
  :demand
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

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

(use-package corfu-quick
  :demand
  :after corfu
  :bind
  (:map corfu-map
        ("C-'" . corfu-quick-insert)
        ("C-;" . corfu-quick-complete))
  :custom
  (corfu-quick1 "asdfghjkl;"))

(use-package corfu-popupinfo
  :demand
  :after corfu
  :custom (corfu-popupinfo-delay '(1.0 . 0.5))
  :config (corfu-popupinfo-mode 1))


(use-package isearch
  :custom
  (isearch-allow-motion t)
  (isearch-allow-scroll 'unlimited)
  (isearch-regexp-lax-whitespace t)
  (isearch-lazy-count t)
  (isearch-repeat-on-direction-change t)
  (isearch-wrap-pause 'no)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format " (%s/%s)")
  (search-whitespace-regexp ".*?"))

(use-package grep
  :custom (grep-use-headings t))

;; wgrep
;; https://github.com/mhayashi1120/Emacs-wgrep
(use-package wgrep
  :ensure
  :custom (wgrep-auto-save-buffer t))

;; avy
;; https://github.com/abo-abo/avy
(use-package avy
  :ensure
  :commands (avy-setup-default)
  :bind ("C-'" . avy-goto-char-timer)
  :config (avy-setup-default))

;; ace-link
;; https://github.com/abo-abo/ace-link
(use-package ace-link
  :ensure
  :commands (ace-link-setup-default)
  :defer 1
  :config (ace-link-setup-default))

(use-package elisp-mode
  :custom (elisp-flymake-byte-compile-load-path (cons "./" load-path)))

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)
        ("C-c ! !" . flymake-show-buffer-diagnostics)
        ("C-c ! p" . flymake-show-project-diagnostics))
  :custom (flymake-mode-line-lighter "FM")
  :hook prog-mode)

;; recentf
(use-package compile
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error))

(use-package recentf
  :defer 1
  :custom
  (recentf-max-saved-items 500)
  :config
  (recentf-mode))

(use-package saveplace
  :defer 1
  :custom (save-place-limit 800)
  :config (save-place-mode 1))

;; saveplace-pdf-view
;; https://github.com/nicolaisingh/saveplace-pdf-view
(use-package saveplace-pdf-view
  :ensure
  :demand
  :after saveplace)

;; rainbow-mode
;; https://elpa.gnu.org/packages/rainbow-mode.html
(use-package rainbow-mode
  :ensure
  :delight
  :hook (conf-mode css-mode Man-mode prog-mode sgml-mode))

(use-package cdlatex
  :ensure
  :custom (cdlatex-math-modify-alist '((?B "\\mathbb" nil t nil nil))))

(use-package tex
  :ensure auctex)

(use-package org
  :ensure
  :pin gnu
  :functions (delight)
  :bind
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  (:map org-mode-map
        ("C-'" . avy-goto-char-timer)
        ([f6] . org-latex-preview)
        ([f7] . insert-char))
  :config
  (delight 'org-indent-mode)
  (setq org-attach-auto-tag nil
        org-confirm-babel-evaluate nil
        org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %TIMESTAMP %SCHEDULED %DEADLINE"
        org-ctrl-k-protect-subtree t
        org-edit-src-content-indentation 0
        org-ellipsis " ..."
        org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t
        org-file-apps (append '(("\\.pdf\\'" . "zathura %s")
                                ("\\.mp4\\'" . "mpv %s")
                                ("\\.webm\\'" . "mpv %s")
                                ("\\.odt\\'" . "libreoffice %s"))
                              org-file-apps)
        org-html-validation-link nil
        org-log-into-drawer t
        org-M-RET-may-split-line '((default . nil))
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((org-agenda-files :maxlevel . 4))
        org-refile-use-outline-path 'file
        org-return-follows-link t
        org-use-speed-commands t
        org-startup-folded t
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WIP(w)" "REDO(r)" "|" "DONE(d)" "CANCELED(c)"))
        org-track-ordered-property-with-tag t
        org-use-fast-tag-selection t
        )
  (add-to-list 'org-modules 'org-mouse)
  ;; (push 'org-drill org-modules)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((C . t)
                                 (ditaa . t)
                                 (emacs-lisp . t)
                                 (gnuplot . t)
                                 (python . t)
                                 (shell . t)))
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "babel" t ("pdflatex")))
  ;; (dolist (i org-level-faces) (set-face-attribute i nil :overline t))
  :hook
  (org-mode . auto-fill-mode)
  (org-mode . turn-on-org-cdlatex))

(use-package org-agenda
  :bind ("C-c a" . org-agenda)
  :custom (org-agenda-sticky t))

(use-package org-capture
  :custom
  (org-capture-templates
   `(("b" "Insert new Book" entry
      (file+headline "~/org/books_movies_series.org" "Books")
      (file "~/org/template/books_template.org")
      :empty-lines-after 2)

     ("m" "Next week menu" entry
      (file+headline "~/org/meals.org"
                     ,(format-time-string "%Y"))
      (file "~/org/template/weekly_meals.org")
      :jump-to-captured t)

     ("y" "Add YouTube channel" entry
      (file+olp "~/.emacs.d/elfeed/elfeed.org"
                "Web" "Youtube")
      "* [[%(s-replace \"channel/\" \"feeds/videos.xml?channel_id=\" \"%x\")][%^{Insert channel name}]]")

     ("s" "New logged org-pomodoro" entry
      (file+olp+datetree "~/org/activities.org"
                         "Log")
      "* %?"
      :jump-to-captured t
      :empty-lines 0
      :tree-type week
      :before-finalize (org-pomodoro))

     ("S" "New activity log (clock in)" entry
      (file+olp+datetree "~/org/activities.org"
                         "Log")
      "* %?"
      :clock-in it
      :clock-keep t
      :jump-to-captured t
      :empty-lines 0
      :tree-type week))))

(use-package org-indent
  :delight
  :custom
  (org-indent-indentation-per-level 1)
  (org-indent-mode-turns-on-hiding-stars nil)
  :hook org-mode)

(use-package gnuplot
  :ensure)

(use-package org-pomodoro
  :ensure
  :bind ("C-c r" . org-pomodoro)
  :custom
  (org-pomodoro-expiry-time 40)
  (org-pomodoro-keep-killed-pomodoro-time t)
  (org-pomodoro-audio-player (concat (executable-find "mpv") " --volume=50"))
  (org-pomodoro-manual-break t))

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

;; org-download
;; https://github.com/abo-abo/org-download
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
  (denote-backlinks-mode . (lambda () (setq-local truncate-lines t)))
  (dired-mode . denote-dired-mode-in-directories))

(use-package vc
  :custom (vc-follow-symlinks t))

;; magit
;; https://magit.vc
(use-package magit
  :ensure
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-delete-by-moving-to-trash nil)
  (magit-status-goto-file-position t)
  :config
  (put 'magit-edit-line-commit 'disabled nil)
  (require 'magit-extras))

(use-package forge
  :ensure
  :after magit)

(use-package epg
  :custom (epg-pinentry-mode 'loopback))

(use-package auth-source-pass
  :defer 1
  :config (auth-source-pass-enable))

(use-package password-store
  :ensure)

;; gitconfig-mode
;; https://github.com/magit/git-modes
(use-package git-modes
  :ensure)

;; diff-hl
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :ensure
  :defer 1
  :custom
  (diff-hl-draw-borders nil)
  :config
  (global-diff-hl-mode)
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode)))

(use-package tempel
  :ensure
  :pin gnu
  :defer 1
  :bind (("M-+" . tempel-insert))
  :custom (tempel-trigger-prefix "<")
  :hook
  ((prog-mode text-mode) .
   (lambda ()
     (setq-local completion-at-point-functions
                 (cons #'tempel-expand
                       completion-at-point-functions)))))


;; autorevert
(use-package autorevert
  :delight auto-revert-mode
  :defer 1
  :config
  (setq auto-revert-avoid-polling t)
  (global-auto-revert-mode 1))

;; eldoc-mode
(use-package eldoc
  :delight
  :custom (eldoc-documentation-strategy 'eldoc-documentation-compose))

(use-package eshell
  :config
  (add-to-list 'eshell-modules-list 'eshell-smart)
  :hook (eshell-mode . (lambda ()
                         (setenv "PAGER" "cat")
                         (setenv "EDITOR" "emacsclient"))))

;; comint-mode
(use-package comint
  :custom
  (comint-prompt-read-only t))

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package shell
  :custom (shell-has-auto-cd t))

(use-package sh-script
  :custom (sh-shell-file "/bin/sh"))

;; pdf-tools
;; https://github.com/politza/pdf-tools
;; Dep poppler poppler-glibc
(use-package pdf-tools
  :ensure
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :bind
  (:map pdf-view-mode-map
        ("]" . pdf-view-scroll-up-or-next-page)
        ("[" . pdf-view-scroll-down-or-previous-page))
  :custom
  (pdf-annot-activate-created-annotations t)
  (pdf-outline-display-labels t)
  (pdf-view-display-size 'fit-page)
  :hook (pdf-view-mode . pdf-tools-enable-minor-modes))

(use-package pdf-view
  :hook (pdf-view-mode . pdf-view-auto-slice-minor-mode))

(use-package nov
  :ensure
  :mode ("\\.epub\\'" . nov-mode))

;; terminal here
;; https://github.com/davidshepherd7/terminal-here
(use-package terminal-here
  :ensure
  :bind
  ("C-c v" . terminal-here-launch)
  :custom (terminal-here-terminal-command 'foot))

;; ispell
(use-package ispell
  :custom
  (ispell-dictionary "en_US")
  (ispell-complete-word-dict (expand-file-name "~/.words_us-it")))

;; apropos
(use-package apropos
  :custom (apropos-do-all t))

;; dictionary
(use-package dictionary
  :custom (dictionary-server "localhost"))

;; fish-mode
(use-package fish-mode
  :ensure)

;; ediff
(use-package ediff
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

;; gdb-mi
(use-package gdb-mi
  :custom
  (gdb-many-windows t)
  (gdb-show-main t))

;; hippie-exp
(use-package hippie-exp
  :bind
  ("M-/" . hippie-expand))

;; abbrev
(use-package abbrev
  :delight
  :hook (prog-mode text-mode))

;; man
(use-package man
  :bind ("C-c k" . man)
  :custom (Man-notify-method 'aggressive))

;; Wolfram.el
;; https://github.com/hsjunnesson/wolfram.el
(use-package wolfram
  :ensure
  :custom
  (wolfram-alpha-app-id (+pass-get-keep-asking "wolfram_alpha_app_id"))
  (wolfram-alpha-magnification-factor 1.5))

;; systemd
;; https://github.com/holomorph/systemd-mode
(use-package systemd
  :ensure
  :mode
  ("\\.service\\'" . systemd-mode)
  ("\\.timer\\'" . systemd-mode))

;; calc
(use-package calc
  :config
  (setq calc-group-char " "
        calc-group-digits t)
  :custom
  (calc-make-windows-dedicated t)
  (calc-multiplication-has-precedence nil))

(use-package calendar
  :custom (calendar-week-start-day 1))

(use-package help
  :config (temp-buffer-resize-mode 1))

(use-package help-fns
  :custom (help-enable-variable-value-editing t)
  :config (put 'help-fns-edit-variable 'disabled nil))

(use-package help-at-pt
  :custom (help-at-pt-display-when-idle t))

(use-package find-func
  :defer 1
  :config (find-function-setup-keys))

;; ledger-mode
;; https://github.com/ledger/ledger-mode
(use-package ledger-mode
  :ensure
  :mode ("\\.ldg\\'" . ledger-mode)
  :bind
  (:map ledger-mode-map ([f6] . (lambda () (interactive)(insert "€"))))
  :custom
  (ledger-copy-transaction-insert-blank-line-after t)
  (ledger-default-date-format "%Y-%m-%d")
  (ledger-highlight-xact-under-point nil)
  (ledger-reconcile-default-commodity "€")
  :hook (ledger-mode . (lambda ()
                         (setq-local corfu-auto nil))))

(use-package ledger-flymake
  :hook (ledger-mode . ledger-flymake-enable))

;; csv-mode
(use-package csv-mode
  :ensure
  :custom (csv-separators '("," ";" "	"))
  :hook (csv-mode . csv-guess-set-separator))

;; vterm
;; https://github.com/akermu/emacs-libvterm
(use-package vterm
  :ensure
  :bind
  ("C-c t" . vterm-other-window)
  (:map project-prefix-map
        ("t" . +vterm-project-other-window))
  :custom
  (vterm-always-compile-module t)
  (vterm-timer-delay nil)
  :config
  (defun +vterm-project-other-window ()
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (vterm-other-window))))

(use-package shr
  :custom
  (shr-use-colors nil)
  (shr-use-fonts nil))

(use-package yaml-mode
  :ensure)

(use-package matlab-mode
  :ensure
  :defines (matlab-shell-command-switches)
  :mode ("\\.m\\'" . matlab-mode)
  :commands matlab-shell
  :custom (matlab-shell-command-switches '("-nodesktop" "-nosplash")))

(use-package rcirc
  :custom
  (rcirc-default-nick "mssdvd")
  (rcirc-default-part-reason "")
  (rcirc-default-quit-reason "")
  (rcirc-default-user-name rcirc-default-nick)
  (rcirc-display-server-buffer nil)
  (rcirc-fill-column
   (lambda () (max fill-column (/ (window-text-width) 2))))
  (rcirc-kill-channel-buffers t)
  (rcirc-omit-unless-requested '("NAMES" "TOPIC"))
  (rcirc-prompt "%t> ")
  (rcirc-reconnect-delay 30)
  (rcirc-server-alist
   `(("chat.sr.ht"
      :port 6697
      :encryption tls
      :user-name ,(concat "mssdvd/liberachat@" (system-name) "-rcirc")
      :password ,(+pass-get-keep-asking "chat.sr.ht/mssdvd"))))
  (rcirc-time-format "%d-%m %H:%M ")
  (rcirc-track-ignore-server-buffer-flag t)
  :hook
  (rcirc-mode . rcirc-track-minor-mode)
  (rcirc-mode . rcirc-omit-mode)
  (rcirc-mode . read-only-mode))

(use-package rcirc-color
  :ensure
  :demand t
  :after rcirc)

(use-package re-builder
  :custom (reb-re-syntax 'string))

(use-package mouse
  :custom (mouse-yank-at-point t)
  :config (context-menu-mode))

(use-package repeat
  :defer 1
  :config (repeat-mode))

(use-package follow
  :bind (:map follow-mode-map
              ([remap scroll-up-command] . follow-scroll-up)
              ([remap scroll-down-command] . follow-scroll-down)))

(use-package bookmark
  :custom (bookmark-save-flag 1))

(use-package paren
  :custom (show-paren-context-when-offscreen 'child-frame))

(use-package eww
  :custom
  (eww-auto-rename-buffer 'title)
  (eww-search-prefix "https://duckduckgo.com/html/?k1=-1&q="))

(use-package project
  :custom (project-kill-buffers-display-buffer-list t))

(use-package tab-bar
  :custom (tab-bar-show 1))

(use-package tmm
  :config (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions))

(use-package olivetti
  :ensure)

(use-package osm
  :ensure
  :custom (osm-copyright nil))

(use-package hexl
  :hook (hexl-mode . read-only-mode))

(use-package pixel-scroll
  :defer 1
  :config (pixel-scroll-precision-mode 1))

(use-package vundo
  :ensure
  :bind ("C-c u" . vundo)
  :custom (vundo-compact-display t))

(use-package undo-hl
  :defer 1
  :delight
  :load-path "~/src/undo-hl"
  :hook (prog-mode text-mode))

(use-package files
  :custom
  (backup-directory-alist
   `(("." . ,(concat user-emacs-directory "backup/"))))
  (require-final-newline t))

(use-package outline
  :hook ((apropos-mode xref-after-update) . outline-minor-mode))

(use-package tmr
  :ensure)



;;
;; Mail
;;

(setq mail-specify-envelope-from t
      user-mail-address "dm@mssdvd.com")

(use-package message
  :custom
  (message-auto-save-directory nil)
  (message-kill-buffer-on-exit t)
  (message-sendmail-envelope-from 'header))

(use-package gnus
  :custom (gnus-article-date-headers '(combined-local-lapsed)))

(use-package mu4e
  :defer 2
  :bind
  ("C-c m" . mu4e)
  (:map mu4e-main-mode-map
        ("q" . bury-buffer)
        ("Q" . mu4e-quit))
  (:map mu4e-view-mode-map
        ("o" . ace-link-mu4e)
        ("S-SPC" . +mu4e-view-scroll-down-or-prev)
        ("<backspace>" . +mu4e-view-scroll-down-or-prev))
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (mm-discouraged-alternatives '("text/html" "text/richtext"))
  (mu4e-bookmarks
   '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?u)
     (:name "Today's messages" :query "date:today..now" :key ?t)
     (:name "Last 7 days" :query "date:7d..now" :key ?w)
     (:name "All Inboxes" :query "maildir:/INBOX/" :hide-unread t :key ?i)
     (:name "Sent" :query "maildir:/Sent/ OR maildir:/Inviata/" :key ?s)
     (:name "Flagged" :query "flag:flagged" :key ?f)))
  (mu4e-change-filenames-when-moving t)
  (mu4e-completing-read-function #'completing-read)
  (mu4e-compose-context-policy nil)
  (mu4e-compose-format-flowed t)
  (mu4e-context-policy 'pick-first)
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-headers-auto-update nil)
  (mu4e-headers-fields
   '((:human-date . 12)
     (:flags . 6)
     (:mailing-list . 10)
     (:from . 22)
     (:thread-subject)))
  (mu4e-headers-include-related nil)
  (mu4e-hide-index-messages t)
  (mu4e-update-interval 300)
  :config

  (defun +mu4e-view-unread-emails-maybe ()
    "If there are unread emails display them in the mu4e headers buffer,
otherwise display the main mu4e buffer."
     (interactive)
     (let ((unread-query "flag:unread AND NOT flag:trashed"))
       (if (= (cl-loop for q in (mu4e-last-query-results)
                     until (string=
                            (decode-coding-string
                             (plist-get q :query) 'utf-8 t)
                            unread-query)
                     finally return (plist-get q :count)) 0)
         (mu4e)
       (mu4e-search unread-query))))

  (defun +mu4e-view-scroll-down-or-prev ()
    "Scroll-down the current message.
If `mu4e-view-scroll-to-next' is non-nil, and we can't scroll-down
anymore, go the previous message."
    (interactive nil mu4e-view-mode)
    (condition-case nil
        (scroll-down)
      (error
       (when mu4e-view-scroll-to-next
         (mu4e-view-headers-prev)))))

  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "mssdvd"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/dm@mssdvd.com" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "dm@mssdvd.com")
                    (mu4e-sent-messages-behavior . sent)
                    (mu4e-drafts-folder . "/dm@mssdvd.com/Drafts")
                    (mu4e-refile-folder . "/dm@mssdvd.com/Archive")
                    (mu4e-sent-folder . "/dm@mssdvd.com/Sent")
                    (mu4e-trash-folder . "/dm@mssdvd.com/Trash")))
          ,(make-mu4e-context
            :name "gmail"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/d.masserut@gmail.com" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "d.masserut@gmail.com")
                    (mu4e-sent-messages-behavior . delete)
                    (mu4e-drafts-folder . "/d.masserut@gmail.com/[Gmail]/Drafts")
                    (mu4e-refile-folder . "/d.masserut@gmail.com/[Gmail]/All Mail")
                    (mu4e-sent-folder . "/d.masserut@gmail.com/[Gmail]/Sent Mail")
                    (mu4e-trash-folder . "/d.masserut@gmail.com/[Gmail]/Bin")))
          ,(make-mu4e-context
            :name "pec"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/dmasserut@pec.it" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "dmasserut@pec.it")
                    (mu4e-drafts-folder . "/dmasserut@pec.it/Bozze")
                    (mu4e-refile-folder . "/dmasserut@pec.it/Archiviata")
                    (mu4e-sent-folder . "/dmasserut@pec.it/Inviata")
                    (mu4e-trash-folder . "/dmasserut@pec.it/Cestino"))))
        mu4e-org-link-query-in-headers-mode t
        mu4e-user-agent-string nil)

  (mu4e t)
  :hook
  (mu4e-index-updated . (lambda ()
                          (when (string= (getenv "XDG_CURRENT_DESKTOP") "sway")
                            (start-process "update mail indicator" nil
                                           "pkill" "-SIGRTMIN+1" "waybar")))))

(use-package mu4e-alert
  :demand
  :after mu4e
  :ensure
  :config
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display))

(use-package sendmail
  :custom
  (send-mail-function #'sendmail-send-it)
  (sendmail-program "/usr/bin/msmtp"))

;;
;; Languages configurations
;;

(use-package eglot
  :ensure
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref t)
  :config
  (setq-default eglot-workspace-configuration
                '((:gopls . ((gofumpt . t)
                             (linkTarget . "godocs.io")
                             (staticcheck . t)
                             (usePlaceholders . t)))))
  :hook
  ((go-mode
    go-dot-mod-mode
    go-dot-work-mode
    rust-ts-mode)
   . (lambda ()
       (eglot-ensure)
       (add-hook 'before-save-hook 'eglot-format nil t))))

;; Go

(use-package go-mode
  :ensure)

;; C/C++
(use-package cc-vars
  :custom
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (other . "linux"))))

;; Python

(use-package python
  :custom
  ;; black-compatible flake8 configuration
  (python-flymake-command '("flake8" "--max-line-length=88" "--extend-ignore=E203" "-"))
  (python-flymake-msg-alist '(("\(^redefinition\|.*unused.*\|used$\)" . :warning)
                              ("^E999" . :error)
                              ("^[EW][0-9]+" . :note))))

;; Lua

(use-package lua-mode
  :ensure)

(use-package rust-mode
  :ensure)
(use-package js
  :hook (js-mode . (lambda () (setq-local indent-tabs-mode nil))))


(put 'erase-buffer 'disabled nil)

;;; .emacs.el ends here
