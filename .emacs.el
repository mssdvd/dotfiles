;;; .emacs.el --- Main emacs config file  -*- lexical-binding: t; flycheck-check-syntax-automatically: (save mode-enabled); -*-
;;; Commentary:

;;; Code:

(setq read-process-output-max (* 1024 1024 4)) ;; 4mb

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defvar use-package-always-defer t)
(defvar use-package-enable-imenu-support t)

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

;; Tab size
(setq-default tab-width 4)

(setq echo-keystrokes 0.1)

;; support PKGBUILD
(add-to-list 'auto-mode-alist '("PKGBUILD" . shell-script-mode))

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

;; Do not ask to save before compilation
(setq compilation-ask-about-save nil)

;; Long lines slowdowns inhibitor
(global-so-long-mode 1)

;; Disable bell
(setq ring-bell-function 'ignore)


(setq enable-recursive-minibuffers t)


;;;;
;; Custom functions (prefixed with ~)
;;;;

(defun ~check-emacs-updates ()
  "Check for Emacs master and packages updates."
  (interactive)
  (list-packages)
  (magit-status "~/src/emacs-git/emacs-git/")
  (magit-fetch-from-upstream "origin" "master"))

(defun ~ranger-launch-here ()
  "Open the current file's directory in ranger."
  (interactive)
  (if default-directory
      (call-process-shell-command "foot -e ranger" (expand-file-name default-directory) 0 nil)
    (error "No `default-directory' to open")))
(keymap-global-set "C-c r" #'~ranger-launch-here)

(defun ~yank-primary ()
  "Insert the primary selection at the position."
  (interactive)
  (let ((primary (gui-get-primary-selection)))
    (push-mark)
    (insert-for-yank primary)))
(keymap-global-set "S-<insert>" #'~yank-primary)

(defun ~rename-buffer-renamed-file (file newname &optional _ok-if-already-exists)
  "Rename buffer visiting FILE to NEWNAME.
Intended as :after advice for `rename-file'."
  (when (called-interactively-p 'any)
    (when-let ((buffer (get-file-buffer file)))
      (with-current-buffer buffer
        (set-visited-file-name newname nil t)))))
(advice-add 'rename-file :after '~rename-buffer-renamed-file)

(defun ~kill-buffer-deleted-file (file &optional _trash)
  "Kill buffer visiting FILE.
Intended as :after advice for `delete-file'."
  (when (called-interactively-p 'any)
    (when-let ((buffer (get-file-buffer file)))
      (kill-buffer buffer))))
(advice-add 'delete-file :after '~kill-buffer-deleted-file)

;;;;
;; use-package
;;;;

;; no-littering
;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :ensure
  :demand
  :functions (no-littering-expand-etc-file-name no-littering-expand-var-file-name)
  :config
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file 'noerror 'nomessage))

;; delight
;; https://savannah.nongnu.org/projects/delight
(use-package delight
  :ensure)

;; gcmh
;; https://gitlab.com/koral/gcmh
(use-package gcmh
  :disabled
  :delight
  :defer nil
  :config
  (setq gcmh-idle-delay 'auto)
  (gcmh-mode 1))

(use-package modus-themes
  :ensure
  :commands (modus-themes-load-themes)
  :functions (modus-themes-load-vivendi)
  :bind ("C-c q" . modus-themes-toggle)
  :custom
  (modus-themes-completions '((matches . (background))))
  (modus-themes-links '(faint))
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-paren-match '(bold intense))
  (modus-themes-region '(bg-only))
  (modus-themes-italic-constructs t)
  :init
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))

(use-package cus-edit
  :custom (custom-unlispify-tag-names nil))

(use-package time
  :custom (display-time-24hr-format t))

(use-package tree-sitter
  :ensure
  :delight
  :defer 1
  :functions (global-tree-sitter-mode)
  :config (global-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure)

(use-package simple
  :bind
  ([remap count-words-region] . count-words)
  ([remap just-one-space] . cycle-spacing)
  ([remap upcase-word] . upcase-dwim)
  ([remap downcase-word] . downcase-dwim)
  ([remap capitalize-word] . capitalize-dwim)
  ([remap count-words-region] . count-words)
  :config
  (setq
   ;; Save existing clipboard text into kill ring before replacing it
   save-interprogram-paste-before-kill t
   ;; Hide commands in M-x which do not work in the current mode
   read-extended-command-predicate #'command-completion-default-include-p)

  ;; enable column number
  (column-number-mode 1)
  ;; display size of the buffer
  (size-indication-mode 1))

(use-package window
  :bind ("M-o" . other-window)
  :config (setq scroll-preserve-screen-position t))

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
  :config (setq display-line-numbers-grow-only t)
  :hook
  ((conf-mode nxml-mode prog-mode yaml-mode) . display-line-numbers-mode))

;; display-fill-column-indicator
(use-package display-fill-column-indicator
  :hook ((conf-mode markdown-mode prog-mode) . display-fill-column-indicator-mode))

;; diff
(use-package diff-mode
  :custom (diff-font-lock-prettify t))

;; dired
(use-package dired
  :config
  (setq dired-listing-switches "-alhv --group-directories-first"
        dired-do-revert-buffer t
        dired-dwim-target t
        dired-kill-when-opening-new-dired-buffer t)
  (put 'dired-find-alternate-file 'disabled nil))

;; dired-x
(use-package dired-x
  :demand
  :after dired
  :config (setq dired-guess-shell-alist-user
                '(("\.pdf$" "zathura")
                  ("\.mp4$" "mpv"))))

(use-package wdired
  :custom (wdired-allow-to-change-permissions t))

(use-package vertico
  :ensure
  :demand
  :commands (vertico-mode)
  :functions (vertico-mouse-mode consult-completion-in-region)
  :defines (vertico-quick1)
  :init (vertico-mode)
  :bind
  ("C-c i" . vertico-repeat)
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)
        ("C-<backspace>" . vertico-directory-delete-word)
        ("C-;" . vertico-quick-insert)
        ("C-'" . vertico-quick-exit))
  :config
  (setq vertico-cycle t
        vertico-quick1 "asdfghjkl;"
        vertico-scroll-margin (/ vertico-count 2)
        completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (vertico-mouse-mode 1)
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  (minibuffer-setup . vertico-repeat-save))

(use-package orderless
  :ensure
  :demand
  :functions (orderless-matching-styles orderless-all-completions orderless-try-completion)
  :config
  (defvar ~orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))
  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * %char-fold char-fold%
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun ~orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) ~orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) ~orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))

  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space
        orderless-matching-styles '(orderless-literal
                                    orderless-regexp
                                    orderless-initialism)
        orderless-style-dispatchers '(~orderless-dispatch)
        read-file-name-completion-ignore-case t))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :ensure t
  :defines (xref-show-xrefs-function xref-show-definitions-function)
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c x" . consult-mode-command)
         ("C-c s" . consult-line)
         ("C-c S" . consult-line-multi)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x C-r" . consult-recent-file)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
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
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("C-c s" . consult-line)
         ("C-c S" . consult-line-multi)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :config
  (setq consult-narrow-key "<")

  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-bookmark
   consult-recent-file consult-xref consult--source-recent-file
   consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  (defvar ~consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      map))
  (consult-customize consult-line :keymap ~consult-line-map))

(use-package consult-flycheck
  :ensure
  :after (consult flycheck)
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

(use-package marginalia
  :ensure
  :demand
  :commands marginalia-mode
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package embark
  :ensure
  :defer 1
  ;; :commands embark-prefix-help-command
  :bind
  ("C-." . embark-act)
  ("C-h B" . embark-bindings)
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure
  :demand
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :demand
  :ensure
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  :config
  (corfu-global-mode))

(use-package isearch
  :config
  (setq isearch-allow-motion t
        isearch-allow-scroll 'unlimited
        isearch-lax-whitespace t
        isearch-regexp-lax-whitespace t
        isearch-lazy-count t
        isearch-repeat-on-direction-change t
        isearch-wrap-pause 'no
        lazy-count-prefix-format nil
        lazy-count-suffix-format " (%s/%s)"
        search-whitespace-regexp ".*?"))

;; wgrep
;; https://github.com/mhayashi1120/Emacs-wgrep
(use-package wgrep
  :ensure
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit))
  :config (setq wgrep-auto-save-buffer t))

;; avy
;; https://github.com/abo-abo/avy
(use-package avy
  :ensure
  :commands (avy-setup-default)
  :bind ("C-'" . avy-goto-char-timer)
  :config (avy-setup-default))

;; avy-flycheck
;; https://github.com/magicdirac/avy-flycheck
(use-package avy-flycheck
  :demand t
  :after flycheck avy
  :config (avy-flycheck-setup))

;; ace-link
;; https://github.com/abo-abo/ace-link
(use-package ace-link
  :ensure
  :commands (ace-link-setup-default)
  :defer 1
  :config (ace-link-setup-default))

;; expand-region.el
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure
  :bind ("C-=" . er/expand-region))

;; flycheck
;; http://www.flycheck.org
(use-package flycheck
  :ensure
  :functions (global-flycheck-mode)
  :defer 1
  :bind
  (:map flycheck-mode-map
        ("M-n" . flycheck-next-error)
        ("M-p" . flycheck-previous-error))
  :config
  (setq flycheck-global-modes '(not org-mode)
        flycheck-mode-line-prefix "FC"
        flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode))

;; flycheck-inline
;; https://github.com/flycheck/flycheck-inline
(use-package flycheck-inline
  :ensure
  :disabled
  :hook (flycheck-mode . flycheck-inline-mode))

;; recentf
(use-package recentf
  :defer 1
  :config
  (setq recentf-max-saved-items 300)
  (recentf-mode))

(use-package saveplace
  :demand t
  :config (save-place-mode 1)
  :custom (save-place-limit 800))

;; saveplace-pdf-view
;; https://github.com/nicolaisingh/saveplace-pdf-view
(use-package saveplace-pdf-view
  :ensure
  :demand
  :after saveplace)

;; rainbow-delimiters
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :ensure
  :hook (prog-mode . rainbow-delimiters-mode))

;; highlight-parentheses
;; https://sr.ht/~tsdh/highlight-parentheses.el/
(use-package highlight-parentheses
  :ensure
  :delight
  :hook
  (prog-mode . highlight-parentheses-mode)
  (minibuffer-setup . highlight-parentheses-minibuffer-setup))

;; rainbow-mode
;; https://elpa.gnu.org/packages/rainbow-mode.html
(use-package rainbow-mode
  :ensure
  :delight
  :hook (conf-mode prog-mode sgml-mode))

(use-package cdlatex
  :ensure
  :custom (cdlatex-math-modify-alist '((?B "\\mathbb" nil t nil nil))))

(use-package auctex
  :ensure)

(use-package org
  :ensure
  :pin gnu
  :functions (delight)
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  (:map org-mode-map
        ("C-'" . avy-goto-char-timer)
        ([f6] . ~org-latex-preview-with-argument))
  :config
  (defun ~org-latex-preview-with-argument ()
    (interactive)
    (let ((current-prefix-arg '(16)))
          (call-interactively #'org-latex-preview)))
  (delight 'org-indent-mode)
  (setq org-attach-auto-tag nil
        org-babel-results-keyword "results"
        org-confirm-babel-evaluate nil
        org-capture-templates `(("b" "Insert new Book" entry
                                 (file+headline "~/org/books_movies_series.org" "Books")
                                 (file "~/org/template/books_template.org")
                                 :empty-lines-after 2)

                                ("m" "Next week menu" entry
                                 (file+headline "~/org/meals.org"
                                                ,(format-time-string "%Y"))
                                 (file "~/org/template/weekly_meals.org")
                                 :jump-to-captured t)

                                ("y" "Add YouTube channel" entry
                                 (file+olp "~/.emacs.d/var/elfeed/rmh-elfeed.org"
                                           "Web" "Youtube")
                                 "* [[%(s-replace \"channel/\" \"feeds/videos.xml?channel_id=\" \"%x\")][%^{Insert channel name}]]")

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
        org-edit-src-content-indentation 0
        org-ellipsis " ..."
        org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t
        org-file-apps (append '(("\\.pdf\\'" . "zathura %s")
                                ("\\.mp4\\'" . "mpv %s")
                                ("\\.webm\\'" . "mpv %s")
                                ("\\.odt\\'" . "libreoffice %s"))
                              org-file-apps)
        org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
        org-html-validation-link nil
        org-log-into-drawer t
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((org-agenda-files :maxlevel . 4))
        org-refile-use-outline-path 'file
        org-return-follows-link t
        org-show-context-detail (append '((tags-tree . local)) org-show-context-detail)
        org-use-speed-commands t
        org-startup-folded t
        org-startup-with-inline-images t
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
  (org-mode . auto-fill-mode))

(use-package org-indent
  :delight
  :custom
  (org-indent-indentation-per-level 1)
  (org-indent-mode-turns-on-hiding-stars nil)
  :hook (org-mode . org-indent-mode))

(use-package gnuplot
  :ensure)

(use-package org-pomodoro
  :ensure
  :bind ("C-c p" . org-pomodoro)
  :config (setq org-pomodoro-expiry-time 40
                org-pomodoro-keep-killed-pomodoro-time t
                org-pomodoro-audio-player (concat (executable-find "mpv") " --volume=50")
                org-pomodoro-manual-break t))

(use-package alert
  :ensure
  :config (setq alert-default-style 'libnotify))

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

;; ox-reveal
;; https://github.com/yjwen/org-reveal
(use-package ox-reveal
  :ensure
  :config
  (setq org-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.6.0/"
        org-reveal-title-slide nil))

;; org-roam
(use-package org-roam
  :ensure
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n k" . org-roam-buffer-display-dedicated)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n r" . org-roam-ref-add)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+startup: latexpreview\n")
           :unnarrowed t)

          ("b" "book notes" plain
           "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n")
           :unnarrowed t))

        org-roam-capture-ref-templates
        '(("r" "ref" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n")
           :unnarrowed t))

        org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

;; org-roam-ui
(use-package org-roam-ui
  :ensure
  :bind ("C-c n u" . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


;; company
;; https://company-mode.github.io/
(use-package company
  :disabled
  :delight
  :defer 1
  :bind
  (([remap indent-for-tab-command] . company-indent-or-complete-common)
   ("C-c y" . company-yasnippet)
   :map company-active-map
   ("C-o" . company-show-location))
  :config
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-tooltip-align-annotations t
        company-show-quick-access t
        company-search-regexp-function #'company-search-words-in-any-order-regexp
        company-selection-wrap-around t
        company-transformers '(company-sort-prefer-same-case-prefix))

  ;; Disable orderless for company
  (define-advice company-capf
      (:around (orig-fun &rest args) set-completion-styles)
    (let ((completion-styles '(basic partial-completion initials)))
      (apply orig-fun args)))

  (global-company-mode)
  ;; (company-tng-mode)
  )

;; company-quickhelp
;; https://github.com/company-mode/company-quickhelp
(use-package company-quickhelp
  :disabled
  :after company
  :config
  (company-quickhelp-mode)
  (setq company-quickhelp-use-propertized-text t))

(use-package company-box
  :disabled
  :after company
  :delight
  :hook (company-mode . company-box-mode))

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

(use-package vc
  :config (setq vc-follow-symlinks t))

;; magit
;; https://magit.vc
(use-package magit
  :ensure
  :config
  (setq magit-diff-refine-hunk 'all
        magit-diff-refine-ignore-whitespace nil
        magit-delete-by-moving-to-trash nil
        magit-repository-directories '(("~/org" . 0)
                                       ("~/uni" . 0))
        magit-status-goto-file-position t)
  (put 'magit-edit-line-commit 'disabled nil)
  (require 'magit-extras))

(use-package forge
  :ensure
  :after magit)

(use-package epg
  :config (setq epg-pinentry-mode 'loopback))

(use-package auth-source
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
  :config
  (global-diff-hl-mode)
  (setq diff-hl-draw-borders nil)
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode)))

;; yasnippet
;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure
  :functions (yas-minor-mode)
  :delight yas-minor-mode
  :defer 1
  ;; :bind
  ;; ((:map yas-minor-mode-map
  ;;        ("C-j" . yas-expand)
  ;;        :map yas-keymap
  ;;        ("C-j" . yas-next-field-or-maybe-expand)))
  :config
  (yas-global-mode 1)
  ;; TODO: I don't think there is any benefit in disabling tab entirelly
  ;; (dolist (keymap (list yas-minor-mode-map yas-keymap))
  ;;   (define-key keymap (kbd "TAB") nil)
  ;;   (define-key keymap [(tab)] nil))
  )

;; yasnippet-snippets
;; https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
    :ensure)

;; which-key
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure
  :delight
  :defer 1
  :config
  (setq which-key-max-description-length nil)
  (which-key-mode))

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
  :config (setq comint-prompt-read-only t))

(use-package shell
  :config (setq shell-has-auto-cd t))

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

;; terminal here
;; https://github.com/davidshepherd7/terminal-here
(use-package terminal-here
  :ensure
  :bind
  ("C-c t" . terminal-here-launch)
  :config (setq terminal-here-terminal-command 'foot))

;; sudo-edit
;; https://github.com/nflath/sudo-edit
(use-package sudo-edit
  :ensure)

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
  :config (setq apropos-do-all t))

;; dictionary
(use-package dictionary
  :config (setq dictionary-server "dict.org"))

;; fish-mode
(use-package fish-mode
  :ensure)

;; ediff
(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

;; gdb-mi
(use-package gdb-mi
  :config (setq gdb-many-windows t
                gdb-show-main t))

;; hippie-exp
(use-package hippie-exp
  :bind
  ("M-/" . hippie-expand))

;; man
(use-package man
  :bind ("C-c k" . man)
  :custom (Man-notify-method 'pushy))

;; Wolfram.el
;; https://github.com/hsjunnesson/wolfram.el
(use-package wolfram
  :ensure
  :config
  (setq wolfram-alpha-app-id (auth-source-pass-get 'secret "wolfram_alpha_app_id")
        wolfram-alpha-magnification-factor 1.5))

;; define-word
;; https://github.com/abo-abo/define-word
(use-package define-word
    :ensure)

;; systemd
;; https://github.com/holomorph/systemd-mode
(use-package systemd
  :mode
  ("\\.service\\'" . systemd-mode)
  ("\\.timer\\'" . systemd-mode))

;; dumb-jump
;; https://github.com/jacktasia/dumb-jump
(use-package dumb-jump)

;; calc
(use-package calc
  :bind
  ("M-#" . quick-calc)
  ("C-M-#" . calc)
  :config
  (setq calc-group-char " "
        calc-group-digits t
        calc-multiplication-has-precedence nil))

;; calendar
(use-package calendar
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
  :ensure
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h F" . helpful-function)
  (:map emacs-lisp-mode-map ("C-c C-d" . helpful-at-point))
  (:map helpful-mode-map ("C-c C-d" . helpful-at-point))
  (:map lisp-interaction-mode-map ("C-c C-d" . helpful-at-point)))


;; lang-tool
;; https://github.com/mhayashi1120/Emacs-langtool
(use-package langtool
    :ensure
  :config
  (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*"
        langtool-mother-tongue "en-US"))

;; elfeed
;; https://github.com/skeeto/elfeed
(use-package elfeed
  :load-path "~/src/elfeed"
  :ensure
  :defines (elfeed-show-entry)
  :functions (elfeed-search-selected elfeed-search-update-entry)
  :bind
  ("C-c e" . elfeed)
  (:map elfeed-search-mode-map
        ("e" . ~elfeed-open-with-eww)
        ("i" . ~elfeed-play-with-mpv)
        ("o" . 'pocket-reader-elfeed-search-add-link))
  (:map elfeed-show-mode-map
        ("e" . ~elfeed-open-with-eww)
        ("i" . ~elfeed-play-with-mpv)
        ("o" . 'pocket-reader-elfeed-search-add-link))
  :custom
  (elfeed-search-title-max-width 100)
  :config
  (setq url-queue-timeout 30)
  
  (defun ~elfeed--play-with-mpv (entry)
    (elfeed-untag entry 'unread)
    (let ((link (or (caar (elfeed-entry-enclosures entry))
                    (elfeed-entry-link entry))))
      (message "Sent to mpv: %s" link)
      (start-process "elfeed-mpv" nil
                     "mpv"
                     "--pause"
                     "--speed=2.0"
                     "--force-window=immediate"
                     "--demuxer-max-bytes=2GiB"
                     "--"
                     link)))

  (defun ~elfeed-play-with-mpv ()
    "Play entry link with mpv."
    (interactive)
    (if (eq major-mode 'elfeed-show-mode)
        (~elfeed--play-with-mpv elfeed-show-entry)
      (progn
        (cl-loop for entry in (elfeed-search-selected)
                 do (~elfeed--play-with-mpv entry))
        (mapc #'elfeed-search-update-entry (elfeed-search-selected))
        (forward-line))))

  (defun ~elfeed-open-with-eww ()
    "Open elfeed entry in eww with `eww-readable'"
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode)
                     elfeed-show-entry
                   (elfeed-search-selected :single))))
      (elfeed-untag entry 'unread)
      (elfeed-search-update-entry entry)
      (eww (elfeed-entry-link entry)))))

;; elfeed-org
;; https://github.com/remyhonig/elfeed-org
(use-package elfeed-org
  :ensure
  :demand
  :after elfeed
  :config (elfeed-org))

;; pocket-reader
;; https://github.com/alphapapa/pocket-reader.el
(use-package pocket-reader
  :ensure
  :load-path "~/src/pocket-reader"
  :commands (pocket-lib-add-urls))

;; ledger-mode
;; https://github.com/ledger/ledger-mode
(use-package ledger-mode
  :ensure
  :mode ("\\.ldg\\'" . ledger-mode)
  :bind
  (:map ledger-mode-map ([f6] . (lambda () (interactive)(insert "€"))))
  :config
  (setq ledger-copy-transaction-insert-blank-line-after t
        ledger-default-date-format "%Y-%m-%d"
        ledger-highlight-xact-under-point nil
        ledger-reconcile-default-commodity "€"))

;; flycheck-ledger
;; https://github.com/purcell/flycheck-ledger
(use-package flycheck-ledger
  :ensure
  :demand
  :after flycheck)

;; csv-mode
(use-package csv-mode
  :ensure
  :custom (csv-separators '("," ";" "	")))

;; vterm
;; https://github.com/akermu/emacs-libvterm
(use-package vterm
  :ensure
  :bind ("C-c v" . vterm-other-window))

(use-package shr
  :config (setq shr-use-colors nil
                shr-use-fonts nil))

(use-package yaml-mode
  :ensure)

(use-package matlab-mode
  :ensure
  :defines (matlab-shell-command-switches)
  :mode ("\\.m\\'" . matlab-mode)
  :commands matlab-shell
  :config (setq matlab-shell-command-switches '("-nodesktop" "-nosplash")))

(use-package circe
  :ensure
  :config
  (setq circe-network-options
        `(("Libera Chat"
           :tls t
           :nick "mssdvd"
           :sasl-username "mssdvd"
           :sasl-password ,(auth-source-pass-get 'secret "libera.chat/mssdvd")))
        circe-reduce-lurker-spam t))

(use-package re-builder
  :config (setq reb-re-syntax 'string))

(use-package mouse
  :config
  (setq mouse-yank-at-point t)
  (context-menu-mode))

(use-package repeat
  :demand
  :config (repeat-mode))

(use-package follow
  :bind (:map follow-mode-map
              ([remap scroll-up-command] . follow-scroll-up)
              ([remap scroll-down-command] . follow-scroll-down)))

(use-package bookmark
  :config (setq bookmark-save-flag 1))

(use-package paren
  :config (setq show-paren-context-when-offscreen 'child-frame))

(use-package compile
  :bind ("C-c b". recompile))

(use-package eww
  :custom
  (eww-auto-rename-buffer 'title)
  (eww-search-prefix "https://duckduckgo.com/html/?k1=-1&q="))

(use-package project
  :custom (project-kill-buffers-display-buffer-list t))

(use-package winner
  :defer 1
  :config (winner-mode))

(use-package tab-bar
  :config (add-to-list 'tab-bar-format #'tab-bar-format-menu-bar))

(use-package treemacs)
(use-package telega
  :ensure
  :custom
  (telega-completing-read-function 'completing-read)
  :hook
  (telega-load . telega-appindicator-mode)
  (telega-load . telega-mode-line-mode)
  (telega-load . telega-notifications-mode))

(use-package tmm
  :config (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions))

(use-package olivetti
  :ensure)

(use-package osm
  :ensure
  :custom (osm-copyright nil))

(use-package dired-hist
  :load-path "~/src/dired-hist"
  :demand
  :after dired
  :bind (:map dired-mode-map
              ("l" . dired-hist-go-back)
              ("r" . dired-hist-go-forward))
  :config (dired-hist-mode 1))


(use-package treemacs
    :ensure)

(use-package treemacs-magit
  :ensure
  :after (treemacs magit))

;;
;; Mail
;;

(setq mail-specify-envelope-from t
      user-mail-address "dm@mssdvd.com")

(use-package message
  :config
  (setq message-auto-save-directory nil
        message-kill-buffer-on-exit t
        message-sendmail-envelope-from 'header))

(use-package notmuch
  :ensure
  :commands (notmuch notmuch-search ~sync-email)
  :bind
  ("C-x m" . notmuch-mua-new-mail)
  ("C-c m" . (lambda ()
               (interactive)
               (if (string= (shell-command-to-string "notmuch count tag:unread") "0\n")
                   (notmuch)
                 (notmuch-search "tag:unread" t))))
  :config
  (setq mail-user-agent 'notmuch-user-agent
        notmuch-draft-folder "dm@mssdvd.com/Drafts"
        notmuch-fcc-dirs
        '(("dm@mssdvd.com" . "dm@mssdvd.com/Sent +mssdvd +sent")
          ("d.masserut@gmail.com" . "\"d.masserut@gmail.com/[Gmail]/Sent Mail\" +gmail +sent")
          ("dmasserut@pec.it" . "dmasserut@pec.it/Inviata +pec +sent"))
        notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
                                 (:name "unread" :query "tag:unread" :sort-order oldest-first :key "u")
                                 (:name "flagged" :query "tag:flagged" :key "f")
                                 (:name "sent" :query "tag:sent" :key "t")
                                 (:name "drafts" :query "tag:draft" :key "d")
                                 (:name "last 3 months" :query "date:\"3M\".." :key "m")
                                 (:name "all mail" :query "*" :key "a"))
        notmuch-search-result-format '(("date" . "%12s ")
                                       ("count" . "%-7s ")
                                       ("authors" . "%-30s ")
                                       ("subject" . "%s ")
                                       ("tags" . "(%s)"))
        notmuch-show-all-tags-list t
        notmuch-show-part-button-default-action 'notmuch-show-view-part)
  (setq-default notmuch-search-oldest-first nil)

  (defun ~sync-email ()
    "Sync emails and update notmuch index"
    (interactive)
    (start-process "sync emails and update notmuch index" nil
                   "systemctl" "--user" "start" "sync_email.service"))
  :hook
  (notmuch-after-tag . (lambda ()
                         (when (string= (getenv "XDG_CURRENT_DESKTOP") "sway")
                           (start-process "update mail indicator" nil
                                          "pkill" "-SIGRTMIN+1" "waybar")))))

;; Links to Notmuch buffers from Org documents
;; https://git.sr.ht/~tarsius/ol-notmuch
(use-package ol-notmuch
  :ensure)

(use-package sendmail
  :config (setq send-mail-function 'sendmail-send-it
                sendmail-program "/usr/bin/msmtp"))

;;
;; Languages configurations
;;

;; lsp

;; lsp-mode
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :ensure
  :commands (lsp lsp-deferred lsp-format-buffer lsp-organize-imports)
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point))
  :custom
  (lsp-keymap-prefix "C-c o")
  (lsp-modeline-code-actions-segments '(count icon name))
  (lsp-enable-semantic-highlighting t)
  (lsp-keep-workspace-alive nil)
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  ((go-mode java-mode) . lsp-deferred)
  ((go-mode java-mode) . (lambda ()
                           (add-hook 'before-save-hook #'lsp-format-buffer t t)
                           (add-hook 'before-save-hook #'lsp-organize-imports t t)))
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-modeline-code-actions-mode))

;; lsp-ui
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)))

(use-package lsp-treemacs
  :ensure
  :demand
  :after lsp
  :custom (lsp-treemacs-sync-mode t))

;; lsp-java
(use-package lsp-java)

;; lsp-pyright
(use-package lsp-pyright)

(use-package dap-mode)

;; Go

(use-package go-mode
  :ensure)

;; Rust

(use-package rustic
  :ensure
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

(put 'erase-buffer 'disabled nil)

;;; .emacs.el ends here
