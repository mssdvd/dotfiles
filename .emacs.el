;;; .emacs.el --- Main emacs config file
;;; Commentary:

;;; Code:

(setq-default gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024 4)) ;; 4mb

(setq comp-async-report-warnings-errors nil)

(define-advice straight--build-native-compile
    (:around (oldfun &rest args) fix-native-comp-test)
  "Properly disable native compilation on unsupported Emacsen."
  (when (and (fboundp 'native-comp-available-p)
             (native-comp-available-p)
             (fboundp 'native-compile-async))
    (apply oldfun args)))

;; bootstrap straight.el
(eval-and-compile
  (defvar bootstrap-version)
  (setq straight-fix-flycheck t)
  ;; (setq straight-vc-git-default-protocol 'ssh)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; use-package
  ;; https://github.com/jwiegley/use-package
  (setq use-package-always-defer t
        ;; use-package-compute-statistics t
        straight-use-package-by-default t)
  (straight-use-package 'use-package))


;; enable column number
(column-number-mode t)

;; display size of the buffer
(size-indication-mode t)

;; disable startup screen
(setq-default inhibit-startup-screen t)

;; change all prompts to y or n
(setq use-short-answers t)

;; revert buffer
;; (bind-key "C-x m" #'revert-buffer)

;; enable up/down case
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable dired-find-alternate-file
(put 'dired-find-alternate-file 'disabled nil)

;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; use tab to indent and complete
(setq tab-always-indent 'complete)

;; Tab size
(setq-default tab-width 4)

(setq echo-keystrokes 0.1)

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; support PKGBUILD
(add-to-list 'auto-mode-alist '("PKGBUILD" . shell-script-mode))

;; disable this fucking keybind
(global-unset-key (kbd "C-x C-z"))

;; disable zap-to-char
(global-unset-key (kbd "M-z"))

;; ibuffer is better
(bind-key "C-x C-b" #'ibuffer)

;; save buffer
(bind-key [f5] #'save-buffer)

;; compile
(bind-key "C-c m" #'recompile)

;; Pasting with middle-click puts the text where the point is
(setq mouse-yank-at-point t)

;; uniquify
(setq uniquify-buffer-name-style 'forward)

;; Delete trailing whitespace before save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Prefer newer files
(setq load-prefer-newer t)

;; Do not ask to save before compilation
(setq compilation-ask-about-save nil)

;; Show parens mode
(show-paren-mode)

;; Long lines slowdowns inhibitor
(global-so-long-mode 1)

(setq enable-recursive-minibuffers t)

(setq vc-follow-symlinks nil)

;;;;
;; My functions
;;;;

(defun ranger-launch-here ()
  "Open the current file's directory in ranger."
  (interactive)
  (if default-directory
      (call-process-shell-command "foot ranger" (expand-file-name default-directory) 0 nil)
    (error "No `default-directory' to open")))
(bind-key "C-c r" #'ranger-launch-here)

(defun yank-primary ()
  "Insert the primary selection at the position."
  (interactive)
  (let ((primary (gui-get-primary-selection)))
    (insert-for-yank primary)))
(bind-key "S-<insert>" #'yank-primary)

;;;;
;; use-package
;;;;

;; no-littering
;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :demand t
  :config
  (setq-default custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file 'noerror))

;; delight
;; https://savannah.nongnu.org/projects/delight
(use-package delight)

(use-package modus-themes
  :bind ("C-c q" . modus-themes-toggle)
  :init
  (setq modus-themes-bold-constructs t
        modus-themes-slanted-constructs t
        modus-themes-completions 'opinionated
        modus-themes-lang-checkers 'intense-foreground
        modus-themes-paren-match 'intese-bold
        modus-themes-region 'bg-only
        modus-themes-org-blocks 'grayscale)
  (modus-themes-load-themes)
  :config
  (defun my-modus-themes-custom-faces ()
    (set-face-attribute 'elfeed-search-title-face nil
                                    :foreground (modus-themes-color 'fg-alt)))
  (with-eval-after-load 'elfeed-search
    (my-modus-themes-custom-faces))
  (modus-themes-load-vivendi)
  :hook (modus-themes-after-load-theme . my-modus-themes-custom-faces))


(use-package elec-pair
  :defer 1
  :config
  (electric-pair-mode 1)
  :hook
  (org-mode . (lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<)
                                   t
                                 (,electric-pair-inhibit-predicate c)))))))

;; display-line-numbers
(use-package display-line-numbers
  :hook ((conf-mode prog-mode) . display-line-numbers-mode))

;; display-fill-column-indicator
(use-package display-fill-column-indicator
  :straight nil
  :hook (prog-mode . display-fill-column-indicator-mode))

;; diff
(use-package diff
  :config (setq-default diff-font-lock-prettify t))

;; dired
(use-package dired
  :straight nil
  :config
  (setq dired-listing-switches "-alhv --group-directories-first"
        dired-dwim-target t))

;; dired-x
(use-package dired-x
  :straight nil
  :demand t
  :after dired
  :config (setq dired-guess-shell-alist-user
                '(("\.pdf$" "zathura")
                  ("\.mp4$" "mpv"))))

(use-package selectrum
  :defer 1
  :bind
  (("C-c i" . selectrum-repeat)
   :map selectrum-minibuffer-map
   ("C-w" . backward-kill-sexp))
  :config
  (setq selectrum-count-style 'current/matches
        selectrum-extend-current-candidate-highlight t
        selectrum-show-indices '(lambda (i)
                                  (progn
                                    (when (= i 10)
                                      (setq i 0))
                                    (format "%d " i))))
  (dotimes (i 10)
    (define-key
      selectrum-minibuffer-map
      (kbd (format "M-%d" (% (1+ i) 10)))
      `(lambda () (interactive)
	     (selectrum-select-current-candidate ,(1+ i)))))
  (selectrum-mode 1))

(use-package prescient
  :demand t
  :after selectrum
  :config (prescient-persist-mode 1))

(use-package selectrum-prescient
  :demand t
  :after selectrum prescient
  :config (selectrum-prescient-mode 1))

(use-package company-prescient
  :demand t
  :after company prescient
  :config (company-prescient-mode 1))

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c x" . consult-mode-command)
         ("C-c f" . my/consult-fd-find)
         ("C-c g" . consult-ripgrep)
         ("C-c l" . consult-locate)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x C-r" . consult-recent-file)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ("M-g e" . consult-error)
         ;; M-s bindings (search-map)
         ("M-s g" . consult-git-grep)
         ;; ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s f" . consult-focus-lines)
         ("M-s s" . consult-isearch)
         ;; Other bindings
         ("C-s" . consult-line)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos))
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-preview)
  :config
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (defun my/consult-fd-find(&optional dir initial)
    (interactive "P")
    (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
      (consult-find dir initial)))
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package consult-flycheck
  :demand t
  :after consult flycheck
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

(use-package marginalia
  :bind
  (:map minibuffer-local-map
        ("C-M-a" . marginalia-cycle))
  :init (marginalia-mode))

(use-package embark
  :demand t
  :after selectrum
  :bind (:map selectrum-minibuffer-map
              ("C-o" . embark-act))
  :config
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator
        embark-quit-after-action nil))


(use-package embark-consult
  :demand t
  :after embark consult
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))


;; wgrep
;; https://github.com/mhayashi1120/Emacs-wgrep
(use-package wgrep
  :bind (:map grep-mode-map
              ("C-c C-p" . wgrep-change-to-wgrep-mode)))

;; avy
;; https://github.com/abo-abo/avy
(use-package avy
  :bind
  ("C-'" . avy-goto-char-timer)
  ("M-g f" . avy-goto-line)
  ("M-g y" . avy-copy-line))

;; avy-flycheck
;; https://github.com/magicdirac/avy-flycheck
(use-package avy-flycheck
  :demand t
  :after flycheck
  :config (avy-flycheck-setup))

;; ace-link
;; https://github.com/abo-abo/ace-link
(use-package ace-link
  :defer 1
  :config (ace-link-setup-default))

;; expand-region.el
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; flycheck
;; http://www.flycheck.org
;; Dep flake8, clang, tidy, csslint
(use-package flycheck
  :defer 1
  :config
  (setq flycheck-global-modes '(not org-mode)
        flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode)
  :hook (flycheck-mode . (lambda ()
                           (setq left-fringe-width 16)
                           (flycheck-refresh-fringes-and-margins))))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

;; flycheck-pos-tip
(use-package flycheck-pos-tip
  :demand t
  :after flycheck
  :config (flycheck-pos-tip-mode))

;; flycheck-inline
;; https://github.com/flycheck/flycheck-inline
(use-package flycheck-inline
  :disabled
  :hook (flycheck-mode . flycheck-inline-mode))

;; recentf
(use-package recentf
  :defer 1
  :config
  (setq-default recentf-max-menu-items 25
                recentf-max-saved-items 300)
  (recentf-mode))

(use-package saveplace
  :demand t
  :config (save-place-mode 1))

;; rainbow-delimiters
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; rainbow-mode
;; https://elpa.gnu.org/packages/rainbow-mode.html
(use-package rainbow-mode
  :delight
  :hook (prog-mode sgml-mode))

(use-package cdlatex)

(use-package auctex)

(use-package org
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  (:map org-mode-map
        ("C-'" . avy-goto-char-timer)
        ([f6] . org-latex-preview-with-argument))
  :config
  (defun org-latex-preview-with-argument ()
    (interactive)
    (setq current-prefix-arg '(16))
    (call-interactively #'org-latex-preview))
  (delight 'org-inden-mode)
  (setq-default
   org-attach-auto-tag nil
   org-confirm-babel-evaluate nil
   org-capture-templates `(("b" "Insert new Book" entry
                            (file+headline "~/org/books_movies_series.org" "Books")
                            (file "~/org/template/books_template.org")
                            :empty-lines-after 2)
                           ("m" "Next week menu" entry
                            (file+headline "~/org/meals.org"
                                           ,(format-time-string "%Y"))
                            (file "~/org/template/weekly_meals.org"))
                           ("y" "Add YouTube channel" entry
                            (file+olp "~/.emacs.d/var/elfeed/rmh-elfeed.org"
                                      "Web" "Youtube")
                            "* [[%(s-replace \"channel/\" \"feeds/videos.xml?channel_id=\" \"%x\")][%^{Inset channel name}]]")
                           ("s" "New activity log" entry
                            (file+olp+datetree "~/org/activities.org"
                                      "Log")
                            "* %?"
                            :jump-to-captured t
                            :empty-lines 0
                            :tree-type week)
                           ("S" "New activity log (clock in)" entry
                            (file+olp+datetree "~/org/activities.org"
                                      "Log")
                            "* %?"
                            :clock-in it
                            :clock-keep t
                            :jump-to-captured t
                            :empty-lines 0
                            :tree-type week))
   org-catch-invisible-edits 'smart
   org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %TIMESTAMP %SCHEDULED %DEADLINE"
   org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar"
   org-ellipsis " ..."
   org-enforce-todo-checkbox-dependencies t
   org-enforce-todo-dependencies t
   org-file-apps (append '(("\\.pdf\\'" . "zathura %s")
                           ("\\.mp4\\'" . "mpv %s")
                           ("\\.webm\\'" . "mpv %s")
                           ("\\.odt\\'" . "libreoffice %s"))
                         org-file-apps)
   org-format-latex-options (plist-put org-format-latex-options :scale 2.5)
   org-html-validation-link nil
   org-image-actual-width (* (default-font-width) fill-column)
   org-indent-indentation-per-level 1
   org-indent-mode-turns-on-hiding-stars nil
   org-log-into-drawer t
   org-outline-path-complete-in-steps nil
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-targets '((org-agenda-files :maxlevel . 4))
   org-refile-use-outline-path 'file
   org-return-follows-link t
   org-show-context-detail (append '((tags-tree . local)) org-show-context-detail)
   org-startup-folded t
   org-startup-with-inline-images t
   org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WIP(w)" "|" "DONE(d)" "CANCELED(c)"))
   org-track-ordered-property-with-tag t
   org-use-fast-tag-selection t
   )
  (add-to-list 'org-modules 'org-mouse)
  ;; (push 'org-drill org-modules)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((C . t)
                                 (emacs-lisp . t)
                                 (ditaa . t)
                                 (python . t)))
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "babel" t ("pdflatex")))
  ;; (dolist (i org-level-faces) (set-face-attribute i nil :overline t))
  :hook
  (org-mode . auto-fill-mode)
  (org-mode . org-indent-mode))


(use-package org-pomodoro
  :bind ("C-c s" . org-pomodoro)
  :config (setq org-pomodoro-expiry-time 40
                org-pomodoro-keep-killed-pomodoro-time t
                org-pomodoro-audio-player (executable-find "mpv")
                org-pomodoro-manual-break t))

(use-package alert
  :config (setq alert-default-style 'libnotify))

(use-package org-caldav
  :config
  (setq org-caldav-url "https://cdav.migadu.com/calendars/dm@mssdvd.com"
        org-caldav-inbox "~/org/calendar.org"
        org-caldav-calendar-id "home"
        org-icalendar-timezone "Europe/Rome"))

;; evil-org
;; https://github.com/Somelauw/evil-org-mode
(use-package evil-org
  :delight
  :demand t
  :after org evil
  :hook (org-mode . evil-org-mode))

;; org-download
;; https://github.com/abo-abo/org-download
(use-package org-download
  :demand t
  :after org
  :config
  (setq  org-download-screenshot-method "grim -g \"$(slurp)\" %s"
         org-download-image-dir "./org_download")
  ;; Needed because new images are not indented
  (advice-add 'org-download-screenshot :after (lambda () (org-redisplay-inline-images))))

;; ox-reveal
;; https://github.com/yjwen/org-reveal
(use-package ox-reveal
  :config
  (setq-default org-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.6.0/"
                org-reveal-title-slide nil))

;; undo-tree
(use-package undo-tree
  :delight
  :commands (global-undo-tree-mode)
  :init (global-undo-tree-mode)
  :config
  (setq-default undo-tree-enable-undo-in-region t
                undo-tree-visualizer-diff t
                undo-tree-visualizer-timestamps t))

;; company
;; https://company-mode.github.io/
(use-package company
  :delight
  :demand t
  :bind
  ([remap indent-for-tab-command] . company-indent-or-complete-common)
  ("C-c y" . company-yasnippet)
  (:map company-active-map
        ([M-tab] . yas-expand)
        ("C-w" . backward-kill-word)
        ("C-o" . company-show-location))
  :config
  (setq-default company-tooltip-align-annotations t
                company-show-numbers t
                company-minimum-prefix-length 1
                company-selection-wrap-around t)
  (global-company-mode)
  (company-tng-mode))

;; company-quickhelp
;; https://github.com/expez/company-quickhelp
(use-package company-quickhelp
  :demand t
  :after company
  :config
  (company-quickhelp-mode)
  (setq-default company-quickhelp-use-propertized-text t))

(use-package company-box
  :disabled
  :delight
  :config (setq company-box-doc-enable nil)
  :hook (company-mode . company-box-mode))

;; projectile
;; https://github.com/bbatsov/projectile
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (setq-default projectile-enable-caching t
                projectile-files-cache-expire 2592000
                projectile-mode-line-function 'my/projectile-default-mode-line)
  (defun my/projectile-default-mode-line ()
    "Report project name and type in the modeline."
    (let ((project-name (projectile-project-name))
          (project-type (projectile-project-type)))
      (if (and project-name project-type)
          (format " [%s:%s]" project-name project-type)
        " Projectile"))))

;; company-math
;; https://github.com/vspinu/company-math
(use-package company-math
  :disabled
  :hook
  (org-mode . (lambda ()
                (setq-local company-backends
                            (append '((company-math-symbols-latex company-latex-commands))
                                    company-backends)
                            company-math-allow-latex-symbols-in-faces t))))

;; ibuffer-projectile
;; https://github.com/purcell/ibuffer-projectile
(use-package ibuffer-projectile
  :demand t
  :after projectile
  :hook (ibuffer . (lambda ()
                     (ibuffer-projectile-set-filter-groups)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic)))))

;; magit
;; https://magit.vc
(use-package magit
  :config
  (setq-default magit-diff-refine-hunk t
                magit-delete-by-moving-to-trash nil
                magit-repository-directories '(("~/.dotfiles" . 0)
                                               ("~/org" . 0)
                                               ("~/uni" . 0))))

(use-package forge
  :demand t
  :after magit)

(use-package epg
  :config (setq epg-pinentry-mode 'loopback))

(use-package auth-source
  :config (auth-source-pass-enable))

;; gitconfig-mode
;; https://github.com/magit/git-modes
(use-package gitconfig-mode)

;; gitignore-mode
;; https://github.com/magit/git-modes
(use-package gitignore-mode)

;; diff-hl
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :disabled
  :defer 1
  :config
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode)
  (diff-hl-dired-mode)
  (diff-hl-margin-mode)
  (setq-default diff-hl-draw-borders nil
                diff-hl-side 'right)
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;; yasnippet
;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :delight yas-minor-mode
  :defer 1
  :bind ([M-tab] . yas-expand)
  :config
  (yas-global-mode 1)
  :hook (org-mode . (lambda () (yas-activate-extra-mode 'latex-mode))))
;; :hook (org-mode . (lambda ()
;;                     (setq yas-buffer-local-condition
;;                           '(if (org-inside-LaTeX-fragment-p)
;;                                '(require-snippet-condition . always)
;;                              t)))))

;; yasnippet-snippets
;; https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets)

;; auto-yasnippet
;; https://github.com/abo-abo/auto-yasnippet
(use-package auto-yasnippet)

;; which-key
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :delight
  :defer 1
  :bind (:map help-map
              ("C-h" . which-key-C-h-dispatch))
  :config (which-key-mode))

;; autorevert
(use-package autorevert
  :delight auto-revert-mode
  :defer 1
  :config
  (setq auto-revert-avoid-polling t)
  (global-auto-revert-mode 1))

;; eldoc-mode
(use-package eldoc
  :delight)

;; comint-mode
(use-package comint
  :straight nil
  :config (setq-default comint-prompt-read-only t))

;; pdf-tools
;; https://github.com/politza/pdf-tools
;; Dep poppler poppler-glibc
(use-package pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-tools-enable-minor-modes))

;; realgud
;; https://github.com/realgud/realgud
(use-package realgud)

;; terminal here
;; https://github.com/davidshepherd7/terminal-here
(use-package terminal-here
  :bind
  ("C-c t" . terminal-here-launch)
  :config (setq-default terminal-here-terminal-command 'foot))

;; sudo-edit
;; https://github.com/nflath/sudo-edit
(use-package sudo-edit)

;; ispell
(use-package ispell
  :config
  (setq ispell-program-name "hunspell")
  ;; (ispell-set-spellchecker-params)
  ;; (ispell-hunspell-add-multi-dic "it_IT,en_US")
  ;; (setq ispell-dictionary "en_US")
  )

;; apropos
(use-package apropos
  :straight nil
  :config (setq-default apropos-do-all t))

;; fish-mode
(use-package fish-mode)

;; ediff
(use-package ediff
  :config
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally))

;; gdb-mi
(use-package gdb-mi
  :config (setq-default gdb-many-windows t
                        gdb-show-main t))

;; hippie-exp
(use-package hippie-exp
  :bind
  ("M-/" . hippie-expand)
  ("C-M-=" . hippie-expand))

;; man
(use-package man
  :config
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t))

;; Wolfram.el
;; https://github.com/hsjunnesson/wolfram.el
(use-package wolfram
  :config
  (setq-default wolfram-alpha-app-id (auth-source-pass-get 'secret "wolfram_alpha_app_id")
                wolfram-alpha-magnification-factor 1.5))

;; define-word
;; https://github.com/abo-abo/define-word
(use-package define-word)

;; systemd
;; https://github.com/holomorph/systemd-mode
(use-package systemd
  :mode
  ("\\.service\\'" . systemd-mode)
  ("\\.timer\\'" . systemd-mode))

;; dumb-jump
;; https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :config
  (setq-default dumb-jump-selector 'ivy))

;; google-translate
;; https://github.com/atykhonov/google-translate
(use-package google-translate)

;; calc
(use-package calc
  :bind
  ("M-#" . quick-calc)
  ("C-M-#" . full-calc)
  :config
  (setq calc-group-digits t))

;; calendar
(use-package calendar
  :config
  (setq-default calendar-week-start-day 1
                holiday-general-holidays
                '((holiday-fixed 1 1 "Capodanno")
                  (holiday-fixed 5 1 "1 Maggio")
                  (holiday-fixed 4 25 "Liberazione")
                  (holiday-fixed 6 2 "Festa Repubblica"))
                holiday-christian-holidays
                '((holiday-fixed 12 8 "Immacolata Concezione")
                  (holiday-fixed 12 25 "Natale")
                  (holiday-fixed 12 26 "Santo Stefano")
                  (holiday-fixed 1 6 "Epifania")
                  (holiday-easter-etc -52 "Giovedì grasso")
                  (holiday-easter-etc -47 "Martedì grasso")
                  (holiday-easter-etc  -2 "Venerdì Santo")
                  (holiday-easter-etc   0 "Pasqua")
                  (holiday-easter-etc  +1 "Lunedì Pasqua")
                  (holiday-fixed 8 15 "Assunzione di Maria")
                  (holiday-fixed 11 1 "Ognissanti"))))

;; helpful
;; https://github.com/wilfred/helpful
(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h F" . helpful-function)
  (:map emacs-lisp-mode-map ("C-c C-d" . helpful-at-point))
  (:map helpful-mode-map ("C-c C-d" . helpful-at-point)))


;; lang-tool
;; https://github.com/mhayashi1120/Emacs-langtool
(use-package langtool
  :config
  (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*"
        langtool-mother-tongue "en-US")
  (with-eval-after-load 'prog-mode
       (unless (featurep 'flyspell) (require 'flyspell))
       (setq langtool-generic-check-predicate
             '(lambda (start end)
                (let* ((f (get-text-property start 'face)))
                  (memq f flyspell-prog-text-faces)))))
  (with-eval-after-load 'org-mode
       (setq langtool-generic-check-predicate
             '(lambda (start end)
                ;; set up for `org-mode'
                (let* ((begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\|example\\|quote\\)")
                       (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\|example\\|quote\\)")
                       (case-fold-search t)
                       (ignored-font-faces '(org-verbatim
                                             org-block-begin-line
                                             org-meta-line
                                             org-tag
                                             org-link
                                             org-level-1
                                             org-document-info))
                       (rlt t)
                       ff
                       th
                       b e)
                  (save-excursion
                    (goto-char start)

                    ;; get current font face
                    (setq ff (get-text-property start 'face))
                    (if (listp ff) (setq ff (car ff)))

                    ;; ignore certain errors by set rlt to nil
                    (cond
                     ((memq ff ignored-font-faces)
                      ;; check current font face
                      (setq rlt nil))
                     ((string-match "^ *- $" (buffer-substring (line-beginning-position) (+ start 2)))
                      ;; dash character of " - list item 1"
                      (setq rlt nil))
                     ((and (setq th (thing-at-point 'evil-WORD))
                           (or (string-match "^=[^=]*=[,.]?$" th)
                               (string-match "^\\[\\[" th)))
                      ;; embedded cde like =w3m= or org-link [[http://google.com][google]] or [[www.google.com]]
                      ;; langtool could finish checking before major mode prepare font face for all texts
                      (setq rlt nil))
                     (t
                      ;; inside source block?
                      (setq b (re-search-backward begin-regexp nil t))
                      (if b (setq e (re-search-forward end-regexp nil t)))
                      (if (and b e (< start e)) (setq rlt nil)))))
                  ;; (if rlt (message "start=%s end=%s ff=%s" start end ff))
                  rlt)))))

;; elfeed
;; https://github.com/skeeto/elfeed
(use-package elfeed
  :bind
  ("C-c e" . elfeed)
  :custom
  ;; (elfeed-sort-order 'ascending)
  (elfeed-search-title-max-width 100)
  :config
  (setq url-queue-timeout 30)
                                        ; https://www.reddit.com/r/emacs/comments/7usz5q/youtube_subscriptions_using_elfeed_mpv_no_browser/dtpqra5/
  (defun elfeed--play-with-mpv (entry)
    (elfeed-untag entry 'unread)
    (message "Sent to mpv: %s" (elfeed-entry-link entry))
    (start-process "elfeed-mpv" nil "mpv" "--speed=2.0" "--force-window=immediate" "--" (elfeed-entry-link entry)))

  (defun elfeed-play-with-mpv ()
    "Play entry link with mpv."
    (interactive)
    (if (eq major-mode 'elfeed-show-mode)
        (elfeed--play-with-mpv elfeed-show-entry)
      (progn
        (cl-loop for entry in (elfeed-search-selected)
                 do (elfeed--play-with-mpv entry))
        (mapc #'elfeed-search-update-entry (elfeed-search-selected))
        (forward-line)))))


;; elfeed-org
;; https://github.com/remyhonig/elfeed-org
(use-package elfeed-org
  :demand t
  :after elfeed
  :config (elfeed-org))

;; pocket-reader
;; https://github.com/alphapapa/pocket-reader.el
(use-package pocket-reader
  :commands (pocket-lib-add-urls))

;; ledger-mode
;; https://github.com/ledger/ledger-mode
(use-package ledger-mode
  :mode ("\\.ldg\\'" . ledger-mode)
  :bind
  (:map ledger-mode-map ([f6] . (lambda () (interactive)(insert "€"))))
  :config
  (setq ledger-default-date-format "%Y-%m-%d"
        ledger-highlight-xact-under-point nil
        ledger-reconcile-default-commodity "€"))

;; flycheck-ledger
;; https://github.com/purcell/flycheck-ledger
(use-package flycheck-ledger
  :demand t
  :after flycheck)

;; csv-mode
(use-package csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

;; vterm
;; https://github.com/akermu/emacs-libvterm
(use-package vterm
  :bind ("C-c v" . vterm))

(use-package shr
  :config (setq shr-width 80
                shr-use-fonts nil))

(use-package hnreader)

(use-package yaml-mode)

(use-package erc
  :config (add-to-list 'erc-modules 'notifications))

(use-package matlab-mode
  :mode ("\\.m\\'" . matlab-mode)
  :commands matlab-shell
  :config (setq matlab-shell-command-switches '("-nodesktop" "-nosplash")))

;;
;; Evil
;;

;; evil-mode
;; https://github.com/emacs-evil/evil
(use-package evil
  :defer 1
  :bind
  (:map evil-ex-search-keymap
        ("C-w" . backward-kill-word))
  (:map evil-motion-state-map
        ("C-6" . mode-line-other-buffer)
        ("K" . man))
  :init
  (setq-default evil-search-module 'evil-search
                evil-symbol-word-search t
                evil-want-keybinding nil
                evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (setq evil-want-fine-undo t
        evil-split-window-below t
        evil-vsplit-window-right t)
  (evil-set-initial-state 'ledger-reconcile-mode 'emacs)
  (evil-set-initial-state 'ivy-occur-mode 'emacs)
  (evil-set-initial-state 'ivy-occur-grep-mode 'emacs)
  (advice-add 'evil-yank
              :around #'(lambda (orig-fn beg end &rest args)
                          (pulse-momentary-highlight-region beg end)
                          (apply orig-fn beg end args))))

;; evil-surrond
;; https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :demand t
  :after evil
  :config (global-evil-surround-mode 1))

;; evil-collection
;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :demand t
  :after evil
  ;; :custom (evil-collection-company-use-tng nil)
  :config
  (mapc (lambda (x) (setq evil-collection-mode-list (delq x evil-collection-mode-list))) '(calc))
  (evil-define-key 'normal elfeed-search-mode-map
    "R" 'elfeed-search-fetch
    "r" 'elfeed-search-update--force)
  (evil-define-key '(normal visual) elfeed-search-mode-map "o" 'elfeed-search-browse-url)
  (evil-define-key 'normal elfeed-search-mode-map
    "c" 'elfeed-search-clear-filter
    "i" 'elfeed-play-with-mpv
    "p" 'pocket-reader-elfeed-search-add-link)
  (evil-define-key 'normal elfeed-show-mode-map
    "o" 'elfeed-show-visit
    "r" 'elfeed-show-refresh
    "i" 'elfeed-play-with-mpv
    "p" 'pocket-reader-elfeed-entry-add-link)
  (evil-collection-init))

;; evil-lion
;; https://github.com/edkolev/evil-lion
(use-package evil-lion
  :demand t
  :after evil
  :config (evil-lion-mode))

;; evil-matchit
;; https://github.com/redguardtoo/evil-matchit
(use-package evil-matchit
  :disabled
  :demand t
  :after evil
  :config (global-evil-matchit-mode 1))

;; evil-nerd-commenter
;; https://github.com/redguardtoo/evil-nerd-commenter
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines)
  :config (evilnc-default-hotkeys nil t))

;; evil-numbers
;; https://github.com/cofi/evil-numbers
(use-package evil-numbers
  :bind
  (("C-c +" . evil-numbers/inc-at-pt)
   ("C-c -" . evil-numbers/dec-at-pt)
   :map evil-normal-state-map
   ("<kp-add>" . evil-numbers/inc-at-pt)
   ("<kp-subtract>" . evil-numbers/dec-at-pt)))

;;
;; Languages configurations
;;

;; lsp

;; lsp-mode
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c o")
  (lsp-modeline-code-actions-segments '(count icon segments))
  (lsp-enable-semantic-highlighting t)
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  (java-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-modeline-code-actions-mode))

;; lsp-ui
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references))
  :config (setq lsp-ui-doc-position 'bottom
                lsp-ui-sideline-show-diagnostics t))

;; lsp-java
(use-package lsp-java)

;; lsp-pyright
(use-package lsp-pyright)

(use-package dap-mode)

;; Go

(use-package go-mode)

;; Rust

(use-package rustic
  :config (setq rustic-lsp-format t))


;; C/C++

(use-package ccls
  :demand t
  :after lsp
  :config
  (setq-default ccls-executable "ccls"))

;; Python

(use-package pyvenv
  :demand t
  :after python
  :hook (python-mode . pyvenv-mode))

(use-package elpy
  :disabled
  :config (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  :hook (python-mode . elpy-enable))

(put 'erase-buffer 'disabled nil)

;; (mapc
;;    (lambda (face)
;;      (when (eq (face-attribute face :weight) 'normal)
;;        (set-face-attribute face nil :weight 'semi-bold)))
;;    (face-list))

;;; .emacs.el ends here
