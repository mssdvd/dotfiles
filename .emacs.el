;;; package --- Summary
;;; Commentary:

;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("MELPA" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; enable column number
(column-number-mode t)

;; display size of the buffer
(size-indication-mode t)

;; disable scrollbar
(scroll-bar-mode -1)

;; disable toolbar
(tool-bar-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; revert buffer
(bind-key "C-x m" #'revert-buffer)

;; enable up/down case
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable dired-find-alternate-file
(put 'dired-find-alternate-file 'disabled nil)

;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; improve comint performance
(setq-default bidi-display-reordering nil)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; use tab to indent and complete
(setq tab-always-indent 'complete)

;; Tab size
(setq-default tab-width 4)

;; C preferences
(setq-default c-default-style "k&r")

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; window title
(setq frame-title-format
      '((:eval (if (buffer-modified-p) "• "))
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; support PKGBUILD
(add-to-list 'auto-mode-alist '("PKGBUILD" . shell-script-mode))

;; disable this fucking keybind
(unbind-key "C-x C-z")

;; ibuffer is better
(bind-key "C-x C-b" #'ibuffer)

;; switch to previous buffer
(bind-key "M-o" #'mode-line-other-buffer)

;; save buffer
(bind-key [f5] #'save-buffer)

;; Pasting with middle-click puts the text where the point is
(setq mouse-yank-at-point t)

;; uniquify
(setq uniquify-buffer-name-style 'forward)

;; add new line at the end of file
(setq require-final-newline t)

;; Delete trailing whitespace before save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Prefer newer files
(setq load-prefer-newer t)

;; Do not ask to save before compilation
(setq compilation-ask-about-save nil)

;; show buffer boundaries
(setq-default indicate-buffer-boundaries 'left)

;; Save position
(save-place-mode 1)

;; electric-pair-mode
(electric-pair-mode 1)

;; Show parens mode
(show-paren-mode)

;;;;
;; My functions
;;;;

(defun switch-highlight-indent-guides-and-whitespace-modes ()
  "Switch between highlight-indent-guides and whitespace modes."
  (interactive)
  (if (get 'switch-highlight-indent-guides-and-whitespace-modes 'state)
      (progn
        (whitespace-mode -1)
        (highlight-indent-guides-mode 1)
        (put 'switch-highlight-indent-guides-and-whitespace-modes 'state nil))
    (progn
      (whitespace-mode 1)
      (highlight-indent-guides-mode -1)
      (put 'switch-highlight-indent-guides-and-whitespace-modes 'state t))))

(defun ranger-launch-here ()
  "Open the current file's directory in ranger."
  (interactive)
  (if default-directory
      (call-process-shell-command "termite -e ranger" (expand-file-name default-directory) 0 nil)
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

;; use-package
;; https://github.com/jwiegley/use-package
(use-package use-package
  :config (setq use-package-always-ensure t))

;; use-package-chords
;; https://github.com/jwiegley/use-package
(use-package use-package-chords
  :config (key-chord-mode 1))

;; no-littering
;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file 'noerror))

;; delight
;; https://savannah.nongnu.org/projects/delight
(use-package delight)

;; kaolin
(use-package kaolin-themes
  :config
  (setq kaolin-themes-italic-comments t
        kaolin-themes-distinct-company-scrollbar t
        kaolin-themes-underline-wave t)
  (load-theme 'kaolin-ocean t))

;; smartparens
;; https://github.com/Fuco1/smartparens
(use-package smartparens-config
  :disabled
  :ensure smartparens
  :delight smartparens-mode
  :defer 1
  :config
  (show-smartparens-global-mode t)
  (smartparens-global-mode t)
  (ad-disable-advice 'company--insert-candidate 'after 'sp-company--insert-candidate)
  (sp-local-pair '(c-mode c++-mode java-mode js2-mode web-mode ccs-mode) "/*" "*/" :post-handlers '((" | " "SPC")
                                                                                                    ("* ||\n[i]""RET")))

  (sp-local-pair '(python-mode rust-mode c-mode c++-mode java-mode js2-mode json-mode web-mode css-mode sh-mode) "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair '(python-mode rust-mode) "[" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair '(python-mode rust-mode) "(" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))

;; ivy
;; https://github.com/abo-abo/swiper
(use-package ivy
  :delight
  :defer 1
  :bind ("C-c i" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-count-format "(%d/%d) "
        ivy-use-selectable-prompt t
        ivy-extra-directories nil))

;; counsel
;; https://github.com/abo-abo/swiper
;; Dep fzf, ripgrep
(use-package counsel
  :delight
  :defer 1
  :bind
  ("C-c C-f" . counsel-find-file)
  ("M-x" . counsel-M-x)
  ("C-c l" . counsel-locate)
  ("C-x C-r" . counsel-recentf)
  ("C-c g" . counsel-rg)
  ("C-c f" . counsel-fzf)
  :config
  (counsel-mode 1)
  (if (executable-find "rg")
      (setq  counsel-grep-base-command "rg -S --no-heading --line-number --color never -- %s %s")
    (setq counsel-grep-base-command "grep -nEi '%s' %s"))
  (setq counsel-find-file-ignore-regexp "\\`\\."
        counsel-rg-base-command "rg -S -z --hidden --no-heading --line-number --color never %s .")
  (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) ""))

;; swiper
;; https://github.com/abo-abo/swiper
;; M-q - swiper-query-replace
;; C-l - swiper-recenter-top-bottom
;; C-' - swiper-avy
;; C-7 - swiper-mc
;; C-c C-f - swiper-toggle-face-matching
(use-package swiper
  :bind
  ("C-s" . counsel-grep-or-swiper)
  ("C-M-s" . swiper-all))

;; hydra
;; https://github.com/abo-abo/hydra
(use-package hydra
  :defer t)

;; ivy-hydra
;; https://github.com/abo-abo/swiper
(use-package ivy-hydra
  :after ivy)

;; wgrep
;; https://github.com/mhayashi1120/Emacs-wgrep
(use-package wgrep
  :defer t)

;; avy
;; https://github.com/abo-abo/avy
(use-package avy
  :bind
  ("C-'" . avy-goto-char-timer)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g y" . avy-copy-line)
  :config (avy-setup-default))

;; avy-flycheck
;; https://github.com/magicdirac/avy-flycheck
(use-package avy-flycheck
  :after flycheck
  :config (avy-flycheck-setup))

;; ace-link
;; https://github.com/abo-abo/ace-link
(use-package ace-link
  :defer 1
  :config (ace-link-setup-default))

;; ace-window
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :bind ("M-[" . ace-window)
  :config
  (setq aw-dispatch-always t
        aw-background nil)
  :custom-face (aw-leading-char-face ((t (:foreground "red" :slant normal)))))

;; expand-region.el
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; move-text
;; https://github.com/emacsfodder/move-text
(use-package move-text
  :bind
  ("<M-up>" . move-text-up)
  ("<M-down>" . move-text-down))

;; treemacs
;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :bind
  ("M-0" . treemacs)
  (:map treemacs-mode-map
        ([mouse-1] . treemacs-single-click-expand-action))
  :config (setq treemacs-show-hidden-files nil))

;; treemacs-evil
;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs-evil
  :after treemacs evil)

;; treemacs-projectile
;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs-projectile
  :after treemacs projectile)

;; flycheck
;; http://www.flycheck.org
;; Dep flake8, clang, tidy, csslint
(use-package flycheck
  :defer 1
  :bind
  (:map flycheck-mode-map ("C-c ! !" . hydra-flycheck/body))
  ("M-g l" . flycheck-list-errors)
  :config
  (global-flycheck-mode)
  (setq-default flycheck-global-modes '(not org-mode))
  ;; hydra
  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
          :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
          :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter  "Filter")
    ("n"  flycheck-next-error             "Next")
    ("p"  flycheck-previous-error         "Previous")
    ("q"  nil                             "Quit")))

;; flycheck-pos-tip
(use-package flycheck-pos-tip
  :after flycheck
  :config (flycheck-pos-tip-mode t))

;; flycheck-inline
;; https://github.com/flycheck/flycheck-inline
(use-package flycheck-inline
  :hook (flycheck-mode . flycheck-inline-mode))

;; recentf
(use-package recentf
  :ensure nil
  :defer 1
  :config
  (recentf-mode)
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 500))

;; highlight-indent-guides
;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :defer 2
  :delight
  :hook
  (prog-mode . highlight-indent-guides-mode)
  (web-mode . (lambda () (highlight-indent-guides-mode -1)))
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top))

;; rainbow-delimiters
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; rainbow-mode
;; https://elpa.gnu.org/packages/rainbow-mode.html
(use-package rainbow-mode
  :delight
  :hook (prog-mode sgml-mode))

;; org
(use-package org
  :pin org
  :defer t
  :bind (:map org-mode-map ([M-tab] . company-complete))
  :config
  (setq org-log-done t)
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
  (require 'mode-local)
  (setq-mode-local org-mode save-interprogram-paste-before-kill t select-enable-clipboard t)
  (defun sort-all-org-entries ()
    (interactive)
    (let ((fun #'(lambda nil
                   (condition-case nil
                       (org-sort-entries nil ?a)
                     (user-error t)))))
      (org-map-entries fun))))

;; ox-reveal
;; https://github.com/yjwen/org-reveal
(use-package ox-reveal
  :defer t
  :config
  (setq org-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.6.0/"
        org-reveal-title-slide nil))

;; paradox
(use-package paradox
  :defer t
  :config
  (setq paradox-github-token t)
  (setq paradox-execute-asynchronously t))

;; undo-tree
(use-package undo-tree
  :delight
  :commands (global-undo-tree-mode)
  :bind
  ("C-z" . undo)
  ("C-S-z" . undo-tree-redo)
  :init (global-undo-tree-mode)
  :config (setq undo-tree-visualizer-timestamps t))

;; company
;; https://company-mode.github.io/
(use-package company
  :delight
  :defer 1
  :bind
  ([remap indent-for-tab-command] . company-indent-or-complete-common)
  ([M-tab] . company-indent-or-complete-common)
  ("C-c y" . company-yasnippet)
  :config
  (setq company-tooltip-align-annotations t
        company-show-numbers t
        company-minimum-prefix-length 2)
  (global-company-mode)
  (defun add-pcomplete-to-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  :hook (org-mode . add-pcomplete-to-capf))

;; company-quickhelp
;; https://github.com/expez/company-quickhelp
(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode)
  (setq company-quickhelp-use-propertized-text t))

;; company-box
;; https://github.com/sebastiencs/company-box
(use-package company-box
  :disabled
  :delight
  :config (setq company-box-enable-icon nil)
  :hook (company-mode . company-box-mode))

;; company-flx
;; https://github.com/PythonNut/company-flx
(use-package company-flx
  :disabled
  :after company
  :config (company-flx-mode +1))

;; company-statistics
;; https://github.com/company-mode/company-statistics
(use-package company-statistics
  :disabled
  :after company
  :config (company-statistics-mode))

;; projectile
;; https://github.com/bbatsov/projectile
(use-package projectile
  :defer 1
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :config
  (projectile-mode)
  (add-to-list 'projectile-project-root-files "platformio.ini")
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-files-cache-expire 2592000
        projectile-mode-line-function '(lambda () (format " [%s:%s]" (projectile-project-name) (projectile-project-type)))))

;; counsel-projectile
;; https://github.com/ericdanan/counsel-projectile
(use-package counsel-projectile
  :after counsel projectile
  :config (counsel-projectile-mode))

;; ibuffer-projectile
;; https://github.com/purcell/ibuffer-projectile
(use-package ibuffer-projectile
  :hook
  (ibuffer . (lambda ()
               (ibuffer-projectile-set-filter-groups)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

;; magit
;; https://magit.vc
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-diff-refine-hunk 'all
        magit-delete-by-moving-to-trash nil
        magit-repository-directories
        '(("~/Documents/school" . 0)
          ("~/Documents/dotfiles" . 0))
        vc-handled-backends (delq 'Git vc-handled-backends)))

;; gitconfig-mode
;; https://github.com/magit/git-modes
(use-package gitconfig-mode
  :defer t)

;; gitignore-mode
;; https://github.com/magit/git-modes
(use-package gitignore-mode
  :defer t)

;; diff-hl
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :defer 1
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  (setq diff-hl-draw-borders nil
        diff-hl-side 'right))

;; gitignore
;; https://github.com/syohex/emacs-gitignore
(autoload 'gitignore "~/build/emacs-gitignore/gitignore.el" "Generate .gitignore file by using gitignore.io API" t nil)

;; git-timemachine
;; https://github.com/pidu/git-timemachine
(use-package git-timemachine
  :defer t)

;; multiple-cursors
;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-M->" . mc/skip-to-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-M-<" . mc/skip-to-previous-like-this)
  ("C-c c" . mc/mark-all-dwim)
  ("M-<down-mouse-1>" . mc/add-cursor-on-click)
  ("C-c o c" . my/create-fake-cursor-at-point)
  ("C-c o m" . multiple-cursors-mode)
  :custom-face (mc/cursor-face ((t (:inherit cursor :foreground "black"))))
  :config
  (defun my/create-fake-cursor-at-point ()
    "Create fake cursor at point whith the keyboard."
    (interactive)
    (require 'multiple-cursors)
    (if (numberp (point))
        ;; is there a fake cursor with the actual *point* right where we are?
        (let ((existing (mc/fake-cursor-at-point (point))))
          (if existing
              (mc/remove-fake-cursor existing)
            (save-excursion
              (goto-char (point))
              (mc/create-fake-cursor-at-point)))))))

;; yasnippet
;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :delight yas-minor-mode
  :defer 1
  :config (yas-global-mode 1))

;; yasnippet-snippets
;; https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
  :defer t)

;; auto-yasnippet
;; https://github.com/abo-abo/auto-yasnippet
(use-package auto-yasnippet
  :defer t)

;; which-key
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :delight
  :defer 1
  :config
  (which-key-mode)
  (bind-key "C-h" #'which-key-C-h-dispatch help-map))

;; platformIO-mode
;; https://github.com/ZachMassia/platformio-mode
;; Dep platformIO-core
(use-package platformio-mode
  :delight
  :hook (c-mode c++-mode)
  :config
  (irony-cdb-autosetup-compile-options))

;; autorevert
(use-package autorevert
  :delight auto-revert-mode
  :defer 1
  :config (global-auto-revert-mode 1))

;; eldoc-mode
(use-package eldoc
  :delight
  :defer t)

;; comint-mode
(use-package comint
  :ensure nil
  :defer t
  :config (setq comint-prompt-read-only t))

;; pdf-tools
;; https://github.com/politza/pdf-tools
;; Dep poppler poppler-glibc
(use-package pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-tools-enable-minor-modes)
  :config
  (bind-key "C-s" #'isearch-forward pdf-view-mode-map)

  ;; workaround for pdf-tools not reopening to last-viewed page of the pdf:
  ;; https://github.com/politza/pdf-tools/issues/18#issuecomment-269515117
  (defun brds/pdf-set-last-viewed-bookmark ()
    (interactive)
    (when (eq major-mode 'pdf-view-mode)
      (bookmark-set (brds/pdf-generate-bookmark-name))))

  (defun brds/pdf-jump-last-viewed-bookmark ()
    (bookmark-set "fake") ; this is new
    (when
        (brds/pdf-has-last-viewed-bookmark)
      (bookmark-jump (brds/pdf-generate-bookmark-name))))

  (defun brds/pdf-has-last-viewed-bookmark ()
    (assoc
     (brds/pdf-generate-bookmark-name) bookmark-alist))

  (defun brds/pdf-generate-bookmark-name ()
    (concat "PDF-LAST-VIEWED: " (buffer-file-name)))

  (defun brds/pdf-set-all-last-viewed-bookmarks ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (brds/pdf-set-last-viewed-bookmark))))

  (add-hook 'kill-buffer-hook #'brds/pdf-set-last-viewed-bookmark)
  (add-hook 'pdf-view-mode-hook #'brds/pdf-jump-last-viewed-bookmark)
  (unless noninteractive  ; as `save-place-mode' does
    (add-hook 'kill-emacs-hook #'brds/pdf-set-all-last-viewed-bookmarks)))

;; realgud
;; https://github.com/realgud/realgud
(use-package realgud
  :defer t
  :config (setq realgud:pdb-command-name "python -m pdb"))

;; abbrev
(use-package abbrev
  :ensure nil
  :delight
  :defer t)

;; amx
;; https://github.com/DarwinAwardWinner/amx
(use-package amx
  :defer t)

;; terminal here
;; https://github.com/davidshepherd7/terminal-here
(use-package terminal-here
  :bind
  ("C-c t" . terminal-here-launch)
  ("C-c e" . terminal-here-project-launch)
  :config (setq terminal-here-terminal-command '("termite")))

;; sudo-edit
;; https://github.com/nflath/sudo-edit
(use-package sudo-edit
  :defer t)

;; ispell
(use-package ispell
  :defer t
  :config
  (setq ispell-program-name "hunspell"
        ispell-local-dictionary "it_IT"
        ispell-local-dictionary-alist
        '(("it_IT" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))

;; apropos
(use-package apropos
  :ensure nil
  :defer t
  :config (setq apropos-do-all t))

;; ediff
(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

;; gdb-mi
(use-package gdb-mi
  :defer t
  :config
  (setq gdb-many-windows t
        gdb-show-main t))

;; hippie-exp
(use-package hippie-exp
  :bind
  ("M-/" . hippie-expand)
  ("C-M-=" . hippie-expand))

;; Wolfram.el
;; https://github.com/hsjunnesson/wolfram.el
(use-package wolfram
  :defer t
  :config (setq wolfram-alpha-app-id "***REMOVED***"))

;; define-word
;; https://github.com/abo-abo/define-word
(use-package define-word
  :defer t)

;; systemd
;; https://github.com/holomorph/systemd-mode
(use-package systemd
  :mode
  ("\\.service\\'" . systemd-mode)
  ("\\.timer\\'" . systemd-mode))

;; exec-path-from-shell
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :defer 1
  :if (memq window-system '(nil x ns))
  :config (exec-path-from-shell-initialize))

;; dumb-jump
;; https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :bind
  ("M-g j" . dumb-jump-go)
  ("M-g o" . dumb-jump-go-other-window)
  ("M-g i" . dumb-jump-go-prompt)
  ("M-g x" . dumb-jump-go-prefer-external)
  ("M-g z" . dumb-jump-go-prefer-external-other-window)
  ("M-g b" . dumb-jump-back)
  ("M-g q" . dumb-jump-quick-look)
  :config
  (setq dumb-jump-selector 'ivy))

;; google-this
;; https://github.com/Malabarba/emacs-google-this
(use-package google-this
  :delight
  :defer 1
  :config (google-this-mode 1))

;; google-translate
;; https://github.com/atykhonov/google-translate
(use-package google-translate
  :defer t)

;; calc
(use-package calc
  :bind
  ("M-#" . quick-calc)
  ("C-M-#" . calc)
  (:map calc-mode-map ("C-S-z" . calc-redo)))

;; eyebrowse
;; https://github.com/wasamasa/eyebrowse
(use-package eyebrowse
  :defer 1
  :config (eyebrowse-mode t))

;; calendar
(use-package calendar
  :defer t
  :config
  (setq calendar-week-start-day 1
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
  (:map emacs-lisp-mode-map ("C-c C-d" . helpful-at-point)))

;; lang-tool
;; https://github.com/mhayashi1120/Emacs-langtool
(use-package langtool
  :defer t
  :config (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*"))

;; zeal-at-point
;; https://github.com/jinzhu/zeal-at-point
;; Dep zeal
(use-package zeal-at-point
  :bind ("C-c d" . zeal-at-point))

;; elfeed
;; https://github.com/skeeto/elfeed
(use-package elfeed
  :defer t)

;; elfeed-org
;; https://github.com/remyhonig/elfeed-org
(use-package elfeed-org
  :after elfeed
  :config (elfeed-org))

;; ledger-mode
;; https://github.com/ledger/ledger-mode
(use-package ledger-mode
  :mode ("\\.ldg\\'" . ledger-mode)
  :bind
  (:map ledger-mode-map ([f6] . my/insert-euro-symbol))
  :config
  (setq ledger-reconcile-default-commodity "€")
  (evil-set-initial-state 'ledger-reconcile-mode 'emacs)
  (defun my/insert-euro-symbol ()
    "Insert € at point"
    (interactive)
    (insert "€")))

;; flycheck-ledger
;; https://github.com/purcell/flycheck-ledger
(use-package flycheck-ledger
  :after flycheck)

;;
;; Evil
;;

;; evil-mode
;; https://github.com/emacs-evil/evil
(use-package evil
  :defer 1
  :bind
  (:map evil-normal-state-map
        ("M-y" . counsel-evil-registers)
        :map evil-insert-state-map
        ("M-y" . counsel-evil-registers))
  :init (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'helpful-mode 'motion)
  (setq evil-complete-next-func 'hippie-expand)
  (bind-chord "jj" #'evil-normal-state evil-insert-state-map))

;; evil-surrond
;; https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

;; evil-collection
;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :after evil
  :custom (evil-collection-company-use-tng nil)
  :config
  (mapc (lambda (x) (setq evil-collection-mode-list (delq x evil-collection-mode-list))) '(calc))
  (evil-collection-init))

;; evil-lion
;; https://github.com/edkolev/evil-lion
(use-package evil-lion
  :after evil
  :config (evil-lion-mode))

;; evil-matchit
;; https://github.com/redguardtoo/evil-matchit
(use-package evil-matchit
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

;; eglot
;; https://github.com/joaotavora/eglot
(use-package eglot
  :disabled
  :bind
  (:map eglot-mode-map
        ([remap save-buffer] . eglot-format-and-save)
        ([remap next-error] . flymake-goto-next-error)
        ([remap previous-error] . flymake-goto-prev-error))
  :config
  (advice-add 'eglot-eldoc-function :around
              (lambda (oldfun)
                (let ((help (help-at-pt-kbd-string)))
                  (if help (message "%s" help) (funcall oldfun)))))
  (defun eglot-format-and-save ()
    (interactive)
    (eglot-format-buffer)
    (save-buffer))
  :hook
  (rust-mode . eglot-ensure)
  (rust-mode . (lambda () (flycheck-mode -1))))

;; lsp


;; lsp-mode
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :commands lsp
  :init (setq lsp-prefer-flymake nil)
  :hook (rust-mode . lsp))

;; lsp-ui
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode
  :bind
  (:map lsp-ui-mode-map
        ([remap save-buffer] . my/lsp-format-and-save)
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references))
  :config
  (setq lsp-ui-doc-position 'bottom)
  (defun my/lsp-format-and-save ()
    (interactive)
    (lsp-format-buffer)
    (save-buffer)))

;; company-lsp
;; https://github.com/tigersoldier/company-lsp
(use-package company-lsp
  :commands company-lsp)

;; lsp-python
;; https://github.com/emacs-lsp/lsp-python
(use-package lsp-python
  :disabled
  :hook (python-mode . lsp-python-enable))

;; Rust

;; rust-mode
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :config (setq rust-match-angle-brackets nil))

;; cargo
;; https://github.com/kwrooijen/cargo.el
(use-package cargo
  :delight cargo-minor-mode
  :bind (:map cargo-minor-mode-map
              ([f6]. cargo-process-check)
              ([f7] . cargo-process-run)
              ([f8] . cargo-process-test))
  :hook (rust-mode . cargo-minor-mode))

;; Web

;; web-mode
;; https://github.com/fxbois/web-mode
;; Dep tidy
(use-package web-mode
  :mode
  ("\\.phtml\\'"      . web-mode)
  ("\\.tpl\\.php\\'"  . web-mode)
  ("\\.php\\'"        . web-mode)
  ("\\.twig\\'"       . web-mode)
  ("\\.html\\'"       . web-mode)
  ("\\.htm\\'"        . web-mode)
  ("\\.[gj]sp\\'"     . web-mode)
  ("\\.as[cp]x?\\'"   . web-mode)
  ("\\.eex\\'"        . web-mode)
  ("\\.erb\\'"        . web-mode)
  ("\\.mustache\\'"   . web-mode)
  ("\\.handlebars\\'" . web-mode)
  ("\\.hbs\\'"        . web-mode)
  ("\\.eco\\'"        . web-mode)
  ("\\.ejs\\'"        . web-mode)
  ("\\.djhtml\\'"     . web-mode)
  :config
  (setq web-mode-enable-engine-detection t)
  (require 'flycheck)
  (flycheck-add-mode 'html-tidy 'web-mode))

;; json-mode
;; https://github.com/joshwnj/json-mode
(use-package json-mode
  :mode
  ("\\.json\\'" . json-mode)
  ("Pipfile.lock" . json-mode))

;; js2-mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))

;; js2-refactor
;; https://github.com/magnars/js2-refactor.el
(use-package js2-refactor
  :delight
  :hook (js2-mode . js2-refactor-mode))

;; tern
;; http://ternjs.net
;; Dep tern
(use-package tern
  :delight
  :hook (js2-mode . tern-mode))

;; company-tern
;; https://github.com/proofit404/company-tern
(use-package company-tern
  :after js2-mode tern company
  :config (add-to-list 'company-backends 'company-tern))

;; company-web
;; https://github.com/osv/company-web
(use-package company-web
  :after web-mode company
  :config (add-to-list 'company-backends 'company-web-html))

;; ac-html-csswatcher
;; https://github.com/osv/ac-html-csswatcher
;; Dep csswatcher (sudo cpan i CSS::Watcher)
;; Remember: add .csswatcher or use projectile
(use-package ac-html-csswatcher
  :disabled
  :after company-web
  :config
  (company-web-csswatcher-setup)
  (company-web-csswatcher+))

;; ac-html-bootstrap
;; https://github.com/osv/ac-html-bootstrap
(use-package ac-html-bootstrap
  :disabled
  :defer t)

;; emmet-mode
;; https://github.com/smihica/emmet-mode#html-abbreviations
(use-package emmet-mode
  :delight
  :bind
  (:map emmet-mode-keymap
        ("C-M->" . emmet-next-edit-point)
        ("C-M-<" . emmet-prev-edit-point))
  :hook (css-mode web-mode)
  :config
  (setq emmet-move-cursor-between-quotes t)
  (unbind-key "<C-return>" emmet-mode-keymap))

;; impatient-mode
;; https://github.com/netguy204/imp.el
(use-package impatient-mode
  :delight
  :hook (web-mode css-mode))
:config
(defun run-impatient ()
  "Attach a browser to Emacs for a impatient instace.  Use `browse-url' to launch a browser."
  (interactive)
  (httpd-start)
  (browse-url (format "http://127.0.0.1:%d/imp" httpd-port)))

;; skewer-mode
;; https://github.com/skeeto/skewer-mode
(use-package skewer-mode
  :delight
  :hook
  (js2-mode)
  (css-mode . skewer-css-mode)
  ;; (add-hook 'web-mode-hook #'skewer-html-mode)
  )

;; Python

;; anaconda-mode
;; https://github.com/proofit404/anaconda-mode
(use-package anaconda-mode
  :delight
  :hook
  (python-mode)
  (python-mode . anaconda-eldoc-mode))

;; company-anaconda
;; https://github.com/proofit404/company-anaconda
(use-package company-anaconda
  :after anaconda-mode company
  :config (add-to-list 'company-backends '(company-anaconda :with company-capf)))

;; yapfify
;; https://github.com/JorisE/yapfify
;; Dep yapf
(use-package yapfify
  :delight yapf-mode
  :hook (python-mode . yapf-mode))

;; py-isort
;; https://github.com/paetzke/py-isort.el
;; Dep python-isort
(use-package py-isort
  :hook (before-save . py-isort-before-save))

;; emacs-traad
;; https://github.com/abingham/emacs-traad
;; M-x traad-install-server
(use-package traad
  :defer t)

;; pipenv.el
;; https://github.com/pwalsh/pipenv.el
;; Dep pipenv
(use-package pipenv
  :hook (python-mode . pipenv-mode))

;; C & C++

;; irony-mode
;; https://github.com/Sarcasm/irony-mode
;; Dep cmake, clang
(use-package irony
  :delight
  :mode ("\\.ino\\'" . c++-mode)
  :hook
  ((c-mode c++-mode) . irony-mode)
  :config
  (irony-cdb-autosetup-compile-options)
  (eval-after-load 'company '(add-to-list 'company-backends '(company-irony-c-headers company-irony))))

;; irony-eldoc
;; https://github.com/ikirill/irony-eldoc
(use-package irony-eldoc
  :hook (c-mode c++-mode))

;; flycheck-irony
;; https://github.com/Sarcasm/flycheck-irony/
(use-package flycheck-irony
  :hook (flycheck-mode . flycheck-irony-setup))

;; company-irony
;; https://github.com/Sarcasm/company-irony
(use-package company-irony
  :defer t
  :config (setq company-backends (delete 'company-semantic company-backends)))

;; company-irony-c-headers
;; https://github.com/hotpxl/company-irony-c-headers
(use-package company-irony-c-headers
  :defer t)

;;; .emacs ends here
