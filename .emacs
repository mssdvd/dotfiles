;;; package --- Summary
;;; Commentary:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
;;; Code:
(add-to-list 'package-archives
			 '("MELPA" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(terminal-here smex nyan-mode yasnippet yapfify which-key use-package undo-tree smooth-scrolling smartparens smart-tabs-mode realgud rainbow-delimiters py-isort platformio-mode pdf-tools paradox nlinum neotree multiple-cursors moe-theme magit ivy-hydra irony-eldoc highlight-symbol highlight-indent-guides flycheck-pos-tip flycheck-irony delight counsel-projectile company-quickhelp company-irony company-c-headers company-anaconda avy-flycheck all-the-icons ace-window)))
 '(pdf-annot-tweak-tooltips nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 120 :width normal))))
 '(paradox-mode-line-face ((t (:inherit mode-line-buffer-id :weight normal)))))

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
(bind-key "C-x m" 'revert-buffer)

;; enable up/down case
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable dired-find-alternate-file
(put 'dired-find-alternate-file 'disabled nil)

;; highlight line
(global-hl-line-mode)

;; enable delete-selection-mode
(delete-selection-mode 1)

;; use tab to indent
(setq tab-always-indent 'complete)

;; C preferences
(setq-default c-default-style "k&r"
			  tab-width 4)
;;(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))

;; gdb
(defvar gdb-many-windows)
(setq gdb-many-windows t)
(defvar gdb-show-main)
(setq gdb-show-main t)

;; backup files
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/saves/"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;; window title
(setq frame-title-format
	  '((:eval (if (buffer-modified-p) "• "))
		(:eval (if (buffer-file-name)
				   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; support PKGBUILD
(add-to-list 'auto-mode-alist '("PKGBUILD" . shell-script-mode))

;; mark current world
(defun my-mark-current-word (&optional arg allow-extend)
  "Put point at beginning of current word, set mark at end.
ARG fa qualcosa, ALLOW-EXTEND altro"
  (interactive "p\np")
  (setq arg (if arg arg 1))
  (if (and allow-extend
		   (or (and (eq last-command this-command) (mark t))
			   (region-active-p)))
	  (set-mark
	   (save-excursion
		 (when (< (mark) (point))
		   (setq arg (- arg)))
		 (goto-char (mark))
		 (forward-word arg)
		 (point)))
	(let ((wbounds (bounds-of-thing-at-point 'word)))
	  (unless (consp wbounds)
		(error "No word at point"))
	  (if (>= arg 0)
		  (goto-char (car wbounds))
		(goto-char (cdr wbounds)))
	  (push-mark (save-excursion
				   (forward-word arg)
				   (point)))
	  (activate-mark))))
(bind-key "C-c m" 'my-mark-current-word)

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

;;;;
;; USE-PACKAGE
;;;;

;; moe-theme
(use-package moe-theme
  :ensure moe-theme
  :config
  (moe-theme-set-color 'red)
  (moe-dark))

;; nlinum
;; https://elpa.gnu.org/packages/nlinum.html
(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode)
  (setq nlinum-format "%4d")
  (setq nlinum-highlight-current-line t))

;; neotree
(use-package neotree
  :ensure t
  :bind ([f8] . neotree-toggle)
  :config (setq neo-theme 'icons))

;; smartparens
(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :config
  (show-smartparens-global-mode t)
  (smartparens-global-mode t)
  ;;(sp-local-pair '(c-mode c++-mode java-mode) "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair '(c-mode c++-mode java-mode) "/*" "*/" :post-handlers '((" | " "SPC")
																		 ("* ||\n[i]""RET")))

  (sp-local-pair '(c-mode c++-mode java-mode) "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (defun my-create-newline-and-enter-sexp (&rest _ignored)
	"Open a new brace or bracket expression, with relevant newlines and indent. "
	(newline)
	(indent-according-to-mode)
	(forward-line -1)
	(indent-according-to-mode)))

;; ivy
;; https://github.com/abo-abo/swiper
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind
  ("C-c r" . ivy-resume)
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) "))

;; counsel
;; https://github.com/abo-abo/swiper
;; Dep ripgrep
(use-package counsel
  :ensure t
  :diminish counsel-mode
  :bind
  ("C-x l" . counsel-locate)
  ("C-x C-r" . counsel-recentf)
  ("C-x g". counsel-rg)
  :config
  (setq counsel-grep-base-command "grep -nEi '%s' %s")
  (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) ""))

;; swiper
;; https://github.com/abo-abo/swiper
;; M-q - swiper-query-replace
;; C-l - swiper-recenter-top-bottom
;; C-' - swiper-avy
;; C-7 - swiper-mc
;; C-c C-f - swiper-toggle-face-matching
(use-package swiper
  :ensure t
  :bind
  ("C-s" . counsel-grep-or-swiper)
  ("C-c s" . swiper-all))

;; ivy-hydra
;; https://github.com/abo-abo/swiper
(use-package ivy-hydra
  :ensure t)

;; avy
;; https://github.com/abo-abo/avy
(use-package avy
  :ensure t
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-timer)
  ("M-g f" . avy-goto-line)
  ;;  ("M-g w" . avy-goto-word-1)
  ("M-g e" . avy-goto-word-0)
  :config (avy-setup-default))

;; avy-flycheck
;; https://github.com/magicdirac/avy-flycheck
(use-package avy-flycheck
  :ensure t
  :config (avy-flycheck-setup))

;; ace-window
;; https://github.com/abo-abo/ace-window
;; x - delete window
;; m - swap (move) window
;; c - split window fairly, either vertically or horizontally
;; v - split window vertically
;; b - split window horizontally
;; n - select the previous window
;; i - maximize window (select which window)
;; o - maximize current window
(use-package ace-window
  :ensure t
  :bind ("M-[" . ace-window)
  :config (setq aw-dispatch-always t))

;; flycheck
;; http://www.flycheck.org
;; Dep flake8, clang
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (setq flycheck-global-modes '(not org-mode)))

;; flycheck-pos-tip
(use-package flycheck-pos-tip
  :ensure t
  :config (flycheck-pos-tip-mode t))

;; recentf
(use-package recentf
  :config
  (recentf-mode)
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 100))

;; highlight-indent-guides
;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

;;rainbow-delimiters
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; org
(use-package org
  :ensure t
  :config (setq org-log-done t))

;; paradox
(use-package paradox
  :ensure t
  :config (setq paradox-github-token t))

;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  :bind
  ("C-z" . undo)
  ("C-S-z" . undo-tree-redo))

;; smooth-scrolling
;; https://github.com/aspiers/smooth-scrolling
(use-package smooth-scrolling
  :ensure t
  :config (smooth-scrolling-mode 1))

;; company
;; https://company-mode.github.io/
(use-package company
  :ensure t
  :diminish company-mode
  :bind ([(M-tab)]. company-complete)
  :config
  (global-company-mode)
  (add-to-list 'company-backends '(company-c-headers company-irony company-anaconda)))

;; company-quickhelp
;; https://github.com/expez/company-quickhelp
(use-package company-quickhelp
  :ensure t
  :config (company-quickhelp-mode 1))

;; projectile
;; https://github.com/bbatsov/projectile
(use-package projectile
  :ensure t
  :delight '(:eval (concat " " (projectile-project-name)))
  :config
  (projectile-mode)
  (add-to-list 'projectile-project-root-files "platformio.ini")
  (setq projectile-completion-system 'ivy
		projectile-enable-caching t))

;; counsel-projectile
;; https://github.com/ericdanan/counsel-projectile
(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-on))

;; irony-mode
;; https://github.com/Sarcasm/irony-mode
;; Dep cmake, clang
(use-package irony
  :ensure t
  :diminish irony-mode
  :config
  (defun irony-and-platformio-hook ()
	(irony-mode)
	(irony-eldoc)
	(platformio-conditionally-enable))
  (add-hook 'c-mode-hook 'irony-and-platformio-hook)
  (add-hook 'c++-mode-hook 'irony-and-platformio-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; irony-eldoc
;; https://github.com/ikirill/irony-eldoc
(use-package irony-eldoc
  :ensure t)

;; flycheck-irony
;; https://github.com/Sarcasm/flycheck-irony/
(use-package flycheck-irony
  :ensure t
  :config (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; company-irony
;; https://github.com/Sarcasm/company-irony
(use-package company-irony
  :ensure t)

;; company-c-headers
;; https://github.com/randomphrase/company-c-headers
(use-package company-c-headers
  :ensure t)

;; smart-tabs-mode
;; http://github.com/jcsalomon/smarttabs
(use-package smart-tabs-mode
  :ensure t
  :config (smart-tabs-insinuate 'c 'c++ 'javascript 'java))

;; magit
;; https://magit.vc
(use-package magit
  :ensure t)

;; multiple-cursors
;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c <" . mc/mark-all-like-this)
  ("M-<down-mouse-1>" . mc/add-cursor-on-click))

;; highlight-symbol
;; https://github.com/nschum/highlight-symbol.el
(use-package highlight-symbol
  :ensure t
  :bind
  ([(C-f5)] . highlight-symbol)
  ([f5] . highlight-symbol-next)
  ([(S-f5)] . highlight-symbol-prev)
  ([(M-f5)] . highlight-symbol-query-replace))

;; yasnippet
;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

;; anaconda-mode
;; https://github.com/proofit404/anaconda-mode
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

;; company-anaconda
;; https://github.com/proofit404/company-anaconda
(use-package company-anaconda
  :ensure t)

;; yapfify
;; https://github.com/JorisE/yapfify
;; Dep yapfy
(use-package yapfify
  :ensure t
  :diminish yapf-mode
  :config (add-hook 'python-mode-hook 'yapf-mode))

;; py-isort
;; https://github.com/paetzke/py-isort.el
;; Dep isort
(use-package py-isort
  :ensure t
  :config (add-hook 'before-save-hook 'py-isort-before-save))

;; all the icons
;; https://github.com/domtronn/all-the-icons.el
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

;; all the icons dired
;; https://github.com/jtbm37/all-the-icons-dired
(use-package all-the-icons-dired
  :ensure t
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; which-key
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

;; platformIO-mode
;; https://github.com/ZachMassia/platformio-mode
;; Dep platformIO-core
(use-package platformio-mode
  :ensure t)

;; autorevert-mode
(use-package auto-revert
  :diminish auto-revert-mode
  :config
  :disabled
  (global-auto-revert-mode 1)
  (setq auto-revert-remote-files t))

;; eldoc-mode
(use-package eldoc
  :diminish eldoc-mode)

;; comint-mode
(use-package comint
  :config
  (setq comint-prompt-read-only t)
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
  (add-hook 'python-shell-first-prompt-hook
			(lambda () (add-hook 'comint-output-filter-functions 'comint-truncate-buffer))))

;; pdf-tools
;; https://github.com/politza/pdf-tools
;; Dep poppler poppler-glibc
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (bind-key "C-s" 'isearch-forward pdf-view-mode-map)
  (add-hook 'pdf-view-mode-hook
			(lambda ()
			  (nlinum-mode -1)))
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

  (add-hook 'kill-buffer-hook 'brds/pdf-set-last-viewed-bookmark)
  (add-hook 'pdf-view-mode-hook 'brds/pdf-jump-last-viewed-bookmark)
  (unless noninteractive  ; as `save-place-mode' does
	(add-hook 'kill-emacs-hook #'brds/pdf-set-all-last-viewed-bookmarks)))

;; realgud
;; https://github.com/realgud/realgud
(use-package realgud
  :ensure t
  :config (setq realgud:pdb-command-name "python -m pdb"))

;; nyan-mode
;; https://github.com/TeMPOraL/nyan-mode
(use-package nyan-mode
  :ensure t)

;; delight
;; https://savannah.nongnu.org/projects/delight
(use-package delight
  :ensure t)

;; abbrev
(use-package abbrev
  :diminish abbrev-mode)

;; smex
;; https://github.com/nonsequitur/smex
(use-package smex
  :ensure t)

;; tramp mode
(use-package tramp
  :config
  ;;  (set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
  (defun sudo-edit-current-file ()
	(interactive)
	(let ((position (point)))
	  (find-alternate-file
	   (if (file-remote-p (buffer-file-name))
		   (let ((vec (tramp-dissect-file-name (buffer-file-name))))
			 (tramp-make-tramp-file-name
			  "sudo"
			  (tramp-file-name-user vec)
			  (tramp-file-name-host vec)
			  (tramp-file-name-localname vec)))
		 (concat "/sudo:root@localhost:" (buffer-file-name))))
	  (goto-char position))))

;; terminal here
;; https://github.com/davidshepherd7/terminal-here
(use-package terminal-here
  :ensure t
  :config (setq terminal-here-terminal-command '("termite")))

;;; .emacs ends here
