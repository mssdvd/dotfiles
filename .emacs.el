;;; package --- Summary
;;; Commentary:

;; (add-to-list 'load-path "~/build/benchmark-init-el/")
;; (require 'benchmark-init-loaddefs)
;; (benchmark-init/activate)

;;; Code:
(require 'package)
(add-to-list 'package-archives
			 '("MELPA" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; custom-stuff goes to another file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

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

;; highlight line
(global-hl-line-mode)

;; improve comint performance
(setq-default bidi-display-reordering nil)

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

;; CLIPBOARD

;; Disable clipboard sync
(setq select-enable-clipboard nil)

(bind-key "C-y" #'clipboard-yank)
(bind-key "C-M-y" #'yank)
(bind-key "C-w" #'clipboard-kill-region)
(bind-key "M-w" #'clipboard-kill-ring-save)

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

;; Save position
(save-place-mode 1)

;;;;
;; My functions
;;;;

(defun my-create-fake-cursor-at-point ()
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
			(mc/create-fake-cursor-at-point))))))

(defun my-copy-line ()
  "Copy current line."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (clipboard-kill-ring-save
     (point)
     (line-end-position)))
  (message "1 line copied"))
(bind-key "C-c k" #'my-copy-line)

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

;;;;
;; Hydra
;;;;

(defhydra hydra-flycheck
  (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
		:post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
		:hint nil)
  "Errors"
  ("f"  flycheck-error-list-set-filter  "Filter")
  ("n"  flycheck-next-error             "Next")
  ("p"  flycheck-previous-error         "Previous")
  ("q"  nil                             "Quit"))
(bind-key "C-c ! !" #'hydra-flycheck/body)

(defhydra hydra-navigate ()
  "Navigate"
  ("h" backward-char "←")
  ("j" next-line     "↓")
  ("k" previous-line "↑")
  ("l" forward-char  "→")
  ("q" nil           "Quit"))
(bind-key "C-c n" #'hydra-navigate/body)

;;;;
;; use-package
;;;;

;; use-package
;; https://github.com/jwiegley/use-package
(use-package use-package
  :config (setq use-package-always-ensure t))

;; delight
;; https://savannah.nongnu.org/projects/delight
(use-package delight)

;; spacemacs-theme
;; https://github.com/nashamri/spacemacs-theme
(use-package spacemacs-common
  :ensure spacemacs-theme
  :config
  (setq spacemacs-theme-comment-bg nil
		spacemacs-theme-comment-italic t)
  (load-theme 'spacemacs-dark t)
  (defvar my:theme 'spacemacs-dark)
  (defvar my:theme-window-loaded nil)
  (defvar my:theme-terminal-loaded nil)
  (if (daemonp)
	  (add-hook 'after-make-frame-functions (lambda (frame)
											  (select-frame frame)
											  (if (window-system frame)
												  (unless my:theme-window-loaded
													(if my:theme-terminal-loaded
														(enable-theme my:theme)
													  (load-theme my:theme t))
													(setq my:theme-window-loaded t))
												(unless my:theme-terminal-loaded
												  (if my:theme-window-loaded
													  (enable-theme my:theme)
													(load-theme my:theme t))
												  (setq my:theme-terminal-loaded t)))))
	(progn
	  (load-theme my:theme t)
	  (if (display-graphic-p)
		  (setq my:theme-window-loaded t)
		(setq my:theme-terminal-loaded t)))))

;; moe-theme
(use-package moe-theme
  :disabled
  :config
  (setq moe-theme-mode-line-color 'red)
  (moe-dark))

;; neotree
(use-package neotree
  :bind ([f8] . neotree-toggle)
  :config (setq neo-theme 'icons))

;; smartparens
;; https://github.com/Fuco1/smartparens
(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :defer 1
  :config
  (show-smartparens-global-mode t)
  (smartparens-global-mode t)
  (ad-disable-advice 'company--insert-candidate 'after 'sp-company--insert-candidate)
  (sp-local-pair '(c-mode c++-mode java-mode js2-mode web-mode ccs-mode) "/*" "*/" :post-handlers '((" | " "SPC")
																									("* ||\n[i]""RET")))

  (sp-local-pair '(c-mode c++-mode java-mode js2-mode web-mode css-mode) "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))

;; ivy
;; https://github.com/abo-abo/swiper
(use-package ivy
  :diminish ivy-mode
  :defer 1
  :commands (ivy-mode)
  :bind ("C-c i" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
		enable-recursive-minibuffers t
		ivy-count-format "(%d/%d) "))

;; counsel
;; https://github.com/abo-abo/swiper
;; Dep ripgrep
(use-package counsel
  :diminish counsel-mode
  :bind
  ("C-c C-f" . counsel-find-file)
  ("M-x" . counsel-M-x)
  ("C-c l" . counsel-locate)
  ("C-x C-r" . counsel-recentf)
  ("C-x g" . counsel-rg)
  ("C-c f" . counsel-file-jump)
  :after (ivy)
  :config
  (counsel-mode 1)
  (if (executable-find "rg")
	  (setq  counsel-grep-base-command "rg -i --no-heading --line-number --color never -- %s %s")
	(setq counsel-grep-base-command "grep -nEi '%s' %s"))
  (setq counsel-find-file-ignore-regexp "\\`\\.")
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
  ("C-c s" . swiper-all))

;; ivy-hydra
;; https://github.com/abo-abo/swiper
(use-package ivy-hydra
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
  :after (flycheck)
  :config (avy-flycheck-setup))

;; ace-window
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :bind ("M-[" . ace-window)
  :config (setq aw-dispatch-always t))

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

;; flycheck
;; http://www.flycheck.org
;; Dep flake8, clang, tidy, csslint
(use-package flycheck
  :defer 1
  :config
  (global-flycheck-mode)
  (setq flycheck-global-modes '(not org-mode)))

;; flycheck-pos-tip
(use-package flycheck-pos-tip
  :after (flycheck)
  :config (flycheck-pos-tip-mode t))

;; recentf
(use-package recentf
  :ensure nil
  :defer t
  :config
  (recentf-mode)
  (setq recentf-max-menu-items 150
		recentf-max-saved-items 150))

;; highlight-indent-guides
;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :commands (highlight-indent-guides-mode)
  :init
  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
  (add-hook 'web-mode-hook (lambda () (highlight-indent-guides-mode -1)))
  :config
  (setq highlight-indent-guides-method 'character))

;; rainbow-delimiters
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; rainbow-mode
;; https://elpa.gnu.org/packages/rainbow-mode.html
(use-package rainbow-mode
  :diminish (rainbow-mode)
  :commands (rainbow-mode)
  :init
  (add-hook 'prog-mode-hook #'rainbow-mode)
  (add-hook 'sgml-mode #'rainbow-mode))

;; org
(use-package org
  :pin gnu
  :defer t
  :config (setq org-log-done t))

;; paradox
(use-package paradox
  :defer t
  :config (setq paradox-github-token t))

;; undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :commands (global-undo-tree-mode)
  :bind
  ("C-z" . undo)
  ("C-S-z" . undo-tree-redo)
  :init (global-undo-tree-mode)
  :config (setq undo-tree-visualizer-timestamps t))

;; smooth-scrolling
;; https://github.com/aspiers/smooth-scrolling
(use-package smooth-scrolling
  :defer 1
  :config (smooth-scrolling-mode 1))

;; company
;; https://company-mode.github.io/
(use-package company
  :diminish company-mode
  :defer 1
  :bind
  ([(M-tab)]. company-complete)
  ("C-c y" . company-yasnippet)
  :config
  (global-company-mode)
  (setq company-tooltip-align-annotations t))

;; company-quickhelp
;; https://github.com/expez/company-quickhelp
(use-package company-quickhelp
  :after (company)
  :config (company-quickhelp-mode 1))

;; company-flx
;; https://github.com/PythonNut/company-flx
(use-package company-flx
  :after (company)
  :config (company-flx-mode +1))

;; company-statistics
;; https://github.com/company-mode/company-statistics
(use-package company-statistics
  :after (company)
  :config (company-statistics-mode))

;; projectile
;; https://github.com/bbatsov/projectile
(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :defer 1
  :config
  (projectile-mode)
  (add-to-list 'projectile-project-root-files "platformio.ini")
  (setq projectile-completion-system 'ivy
		projectile-enable-caching t
		projectile-track-known-projects-automatically nil
		projectile-files-cache-expire 2592000))

;; counsel-projectile
;; https://github.com/ericdanan/counsel-projectile
(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-on))

;; smart-tabs-mode
;; http://github.com/jcsalomon/smarttabs
(use-package smart-tabs-mode
  :defer 1
  :config (smart-tabs-insinuate 'c 'c++ 'javascript 'java))

;; magit
;; https://magit.vc
(use-package magit
  :bind ("C-c g" . magit-status)
  :config
  (setq magit-repository-directories
		'(("~/Documents/school" . 3)
		  ("~/Documents/dotfiles" . 3))))

;; magithub
;; https://github.com/vermiculus/magithub
(use-package magithub
  :after magit
  :config (magithub-feature-autoinject t))

;; gitconfig-mode
;; https://github.com/magit/git-modes
(use-package gitconfig-mode
  :defer t)

;; gitignore-mode
;; https://github.com/magit/git-modes
(use-package gitignore-mode
  :defer t)

;; gitignore
;; https://github.com/syohex/emacs-gitignore
(autoload 'gitignore "~/build/emacs-gitignore/gitignore.el" "Generate .gitignore file by using gitignore.io API" t nil)

;; multiple-cursors
;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c c" . mc/mark-all-dwim)
  ("M-<down-mouse-1>" . mc/add-cursor-on-click)
  ("C-c o c" . my-create-fake-cursor-at-point)
  ("C-c o m" . multiple-cursors-mode))

;; highlight-symbol
;; https://github.com/nschum/highlight-symbol.el
(use-package highlight-symbol
  :bind
  ([(C-f5)] . highlight-symbol)
  ([f5] . highlight-symbol-next)
  ([(S-f5)] . highlight-symbol-prev)
  ([(M-f5)] . highlight-symbol-query-replace))

;; yasnippet
;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :defer 1
  :config (yas-global-mode 1))

;; auto-yasnippet
;; https://github.com/abo-abo/auto-yasnippet
(use-package auto-yasnippet
  :defer t)

;; all the icons
;; https://github.com/domtronn/all-the-icons.el
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :defer t)

;; all the icons dired
;; https://github.com/jtbm37/all-the-icons-dired
(use-package all-the-icons-dired
  :commands (all-the-icons-dired-mode)
  :init (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

;; which-key
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :diminish which-key-mode
  :defer 1
  :config
  (which-key-mode)
  (bind-key "C-h" #'which-key-C-h-dispatch help-map))

;; platformIO-mode
;; https://github.com/ZachMassia/platformio-mode
;; Dep platformIO-core
(use-package platformio-mode
  :diminish platformio-mode
  :commands (platformio-mode)
  :init
  (add-hook 'c-mode-hook #'platformio-mode)
  (add-hook 'c++-mode-hook #'platformio-mode))

;; autorevert
(use-package autorevert
  :diminish auto-revert-mode
  :config (global-auto-revert-mode 1))

;; eldoc-mode
(use-package eldoc
  :diminish eldoc-mode
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
  :init (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)
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
  :diminish abbrev-mode
  :defer t)

;; smex
;; https://github.com/nonsequitur/smex
(use-package smex
  :defer t)

;; terminal here
;; https://github.com/davidshepherd7/terminal-here
(use-package terminal-here
  :bind ("C-c t" . terminal-here-launch)
  :config (setq terminal-here-terminal-command '("tilix")))

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
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

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
  :defer 1
  :config
  (dumb-jump-mode)
  (setq dumb-jump-selector 'ivy))

;; google-this
;; https://github.com/Malabarba/emacs-google-this
(use-package google-this
  :diminish google-this-mode
  :defer 1
  :config (google-this-mode 1))

;; google-translate
;; https://github.com/atykhonov/google-translate
(use-package google-translate
  :defer t)

;; google-maps
;; https://github.com/jd/google-maps.el
(use-package google-maps
  :defer t
  :bind
  (:map google-maps-static-mode-map
		("h" . google-maps-static-move-west)
		("j" . google-maps-static-move-south)
		("k" . google-maps-static-move-north)
		("l" . google-maps-static-move-east)))


;;
;; Languages configurations
;;

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
  :config (flycheck-add-mode 'html-tidy 'web-mode))

;; js2-mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))

;; js2-refactor
;; https://github.com/magnars/js2-refactor.el
(use-package js2-refactor
  :diminish js2-refactor-mode
  :defer t
  :init (add-hook 'js2-mode-hook #'js2-refactor-mode))

;; tern
;; http://ternjs.net
(use-package tern
  :diminish tern-mode
  :commands (tern-mode)
  :init (add-hook 'js2-mode-hook (lambda () (tern-mode))))

;; company-tern
;; https://github.com/proofit404/company-tern
(use-package company-tern
  :after (js2-mode tern company)
  :config
  (add-to-list 'company-backends 'company-tern))

;; company-web
;; https://github.com/osv/company-web
(use-package company-web
  :after (web-mode company)
  :config (add-to-list 'company-backends 'company-web-html))

;; ac-html-csswatcher
;; https://github.com/osv/ac-html-csswatcher
;; Dep csswatcher (sudo cpan i CSS::Watcher)
;; Remember: add .csswatcher or use projectile
(use-package ac-html-csswatcher
  :after (company-web)
  :config
  (company-web-csswatcher-setup)
  (company-web-csswatcher+))

;; ac-html-bootstrap
;; https://github.com/osv/ac-html-bootstrap
(use-package ac-html-bootstrap
  :defer t)

;; emmet-mode
;; https://github.com/smihica/emmet-mode#html-abbreviations
(use-package emmet-mode
  :diminish emmet-mode
  :commands (emmet-mode)
  :bind
  (:map emmet-mode-keymap
		("C-M->" . emmet-next-edit-point)
		("C-M-<" . emmet-prev-edit-point))
  :init
  (add-hook 'css-mode-hook #'emmet-mode)
  (add-hook 'web-mode-hook #'emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes t)
  (unbind-key "<C-return>" emmet-mode-keymap))

;; impatient-mode
;; https://github.com/netguy204/imp.el
(use-package impatient-mode
  :diminish (impatient-mode)
  :commands (impatient-mode)
  :init
  (add-hook 'web-mode-hook #'impatient-mode)
  (add-hook 'css-mode-hook #'impatient-mode))
:config
(defun run-impatient ()
  "Attach a browser to Emacs for a impatient instace.  Use `browse-url' to launch a browser."
  (interactive)
  (httpd-start)
  (browse-url (format "http://127.0.0.1:%d/imp" httpd-port)))

;; skewer-mode
;; https://github.com/skeeto/skewer-mode
(use-package skewer-mode
  :diminish (skewer-mode)
  :commands (skewer-mode skewer-css-mode)
  :init
  (add-hook 'js2-mode-hook #'skewer-mode)
  (add-hook 'css-mode-hook #'skewer-css-mode)
  ;; (add-hook 'web-mode-hook #'skewer-html-mode)
  )

;; Python

;; anaconda-mode
;; https://github.com/proofit404/anaconda-mode
(use-package anaconda-mode
  :diminish anaconda-mode
  :commands (anaconda-mode anaconda-eldoc-mode)
  :init
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode))

;; company-anaconda
;; https://github.com/proofit404/company-anaconda
(use-package company-anaconda
  :after (anaconda-mode company)
  :config (add-to-list 'company-backends '(company-anaconda :with company-capf)))

;; yapfify
;; https://github.com/JorisE/yapfify
;; Dep yapf
(use-package yapfify
  :diminish yapf-mode
  :commands (yapf-mode)
  :init (add-hook 'python-mode-hook #'yapf-mode))

;; py-isort
;; https://github.com/paetzke/py-isort.el
;; Dep isort
(use-package py-isort
  :commands (py-isort-before-save)
  :init (add-hook 'before-save-hook #'py-isort-before-save))

;; C & C++

;; irony-mode
;; https://github.com/Sarcasm/irony-mode
;; Dep cmake, clang
(use-package irony
  :diminish irony-mode
  :commands (irony-mode)
  :init
  (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)
  :config (eval-after-load 'company '(add-to-list 'company-backends '(company-irony-c-headers company-irony))))

;; irony-eldoc
;; https://github.com/ikirill/irony-eldoc
(use-package irony-eldoc
  :commands (irony-eldoc)
  :init
  (add-hook 'c-mode-hook #'irony-eldoc)
  (add-hook 'c++-mode-hook #'irony-eldoc))

;; flycheck-irony
;; https://github.com/Sarcasm/flycheck-irony/
(use-package flycheck-irony
  :after (flycheck irony)
  :defer t
  :config (add-to-list 'flycheck-checkers 'irony))

;; company-irony
;; https://github.com/Sarcasm/company-irony
(use-package company-irony
  :defer t
  :config (setq company-backends (delete 'company-semantic company-backends)))

;; company-c-headers
;; https://github.com/randomphrase/company-c-headers
(use-package company-c-headers
  :disabled
  :defer t)

;; company-irony-c-headers
;; https://github.com/hotpxl/company-irony-c-headers
(use-package company-irony-c-headers
  :defer t)

;; rtags
;; https://github.com/Andersbakken/rtags
(use-package rtags
  :after (irony)
  :config
  ;; (setq rtags-completions-enabled t)
  (setq rtags-autostart-diagnostics t)
  (rtags-enable-standard-keybindings))
;; (rtags-set-periodic-reparse-timeout 1.0))

;; company-rtags
;; https://github.com/Andersbakken/rtags
;; (use-package company-rtags
;;   :defer t)

;; flycheck-rtags
;; https://github.com/Andersbakken/rtags
;; (use-package flycheck-rtags
;;   :defer t
;;   :init
;;   (defun my-flycheck-rtags-setup ()
;;	(flycheck-select-checker 'rtags)
;;	(setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;	(setq-local flycheck-check-syntax-automatically nil))
;;   (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup))

;; flycheck-clag-analyzer
;; https://github.com/alexmurray/flycheck-clang-analyzer
(use-package flycheck-clang-analyzer
  :after (flycheck irony)
  :defer t
  :config (flycheck-clang-analyzer-setup))

;; cmake-ide
;; https://github.com/atilaneves/cmake-ide
(use-package cmake-ide
  :after (rtags)
  :config (cmake-ide-setup))

;;; .emacs ends here
