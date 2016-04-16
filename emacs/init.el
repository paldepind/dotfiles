;;; init.el --- My init file -*- lexical-binding: t -*-

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq-default scroll-bar-width 6)
(set-scroll-bar-mode 'right)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq use-package-always-ensure t)
(setq sentence-end-double-space nil)
(setq confirm-kill-emacs 'y-or-n-p)
(setq scheme-program-name "petite")

(delete-selection-mode 1) ;; typed text replaces active selection-coding-system
(setq-default indent-tabs-mode nil) ;; use spaces for indentation

(global-set-key (kbd "<f9>") 'previous-buffer)
(global-set-key (kbd "<f10>") 'next-buffer)

;; (exec-path-from-shell-initialize)
;; (exec-path-from-shell-copy-env "NIX_GHC")
;; (exec-path-from-shell-copy-env "NIX_GHCPKG")
;; (exec-path-from-shell-copy-env "NIX_GHC_DOCDIR")
;; (exec-path-from-shell-copy-env "NIX_GHC_LIBDIR")

;; scroll one line at a time (less "jumpy" than defaults)
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

;; (use-package smooth-scroll
;;   :config
;;   )

;; animated scrolling
(use-package sublimity
  :init
  (setq sublimity-auto-hscroll-mode nil
        auto-hscroll-mode t)
  (require 'sublimity-scroll)
  :config
  (setq sublimity-auto-hscroll-mode nil
        auto-hscroll-mode t)
  (sublimity-mode 1))

;; enjoyable mouse scrolling
(setq mouse-wheel-scroll-amount '(2)) ;; two lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Visuals
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 105
                    :weight 'normal
                    :width 'normal)

(load-theme 'leuven t)
;; (setq color-themes '())
;; (use-package color-theme-solarized
;;   :config
;;   (load-theme 'solarized t))

;; (use-package smart-mode-line
;;   :config
;;   (setq sml/theme 'respectful
;; 	sml/no-confirm-load-theme t
;; 	sml/shorten-directory t
;; 	sml/name-width '(32 . 48)
;; 	sml/shorten-modes t
;; 	sml/use-projectile-p 'before-prefixes
;; 	sml/projectile-replacement-format "[%s]")
;;   (sml/setup))

(use-package undo-tree
  :diminish ""
  :config
  (setq undo-tree-visualizer-relative-timestamps t
        undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

;; (require 'composable)
(use-package composable
  :ensure nil
  :load-path "~/projects/composable.el"
  :config
  (composable-mode)
  (composable-mark-mode)
  (composable-def '(smart-comment-region))
  (define-key composable-object-mode-map "'" 'avy-goto-char-in-line)
  (define-key composable-mode-map (kbd "M-;") 'composable-smart-comment-region))

(defun my-resize-margins ()
  (interactive)
  (let ((margin-size (/ (- (window-width) 80) 2)))
    (set-window-margins nil margin-size margin-size)))

;; Session/desktop
;; (desktop-save-mode 1)

;; Evil
;; (setq evil-want-C-u-scroll t)
;; (require 'evil)

;; (global-evil-leader-mode)

;; (evil-leader/set-leader "<SPC>")
;; (evil-leader/set-key
  ;; "s" 'save-buffer
  ;; "m" 'helm-mini
  ;; "f" 'helm-find-files
  ;; "x" 'helm-M-x
;; )

;; (evil-mod;; (define-key evil-normal-state-map "e" 'evil-next-line)
;; (define-key evil-normal-state-map "i" 'evil-previous-line)
;; (define-key evil-normal-state-map "o" 'evil-forward-char)

;; (define-key evil-normal-state-map "h" 'evil-search-next)
;; (define-key evil-normal-state-map "j" 'evil-forward-word-end)
;; (define-key evil-normal-state-map "k" 'evil-insert)
;; (define-key evil-normal-state-map "l" 'evil-open-below)

;; (define-key evil-insert-state-map (kbd "M-n") 'evil-force-normal-state)

(define-key input-decode-map (kbd "C-m") (kbd "H-m"))
(global-set-key (kbd "H-m") 'helm-mini)

;; god-mode
(define-key input-decode-map (kbd "C-i") (kbd "H-i")) ; Allow binding to C-i as H-i

(use-package god-mode
  :config
  (global-set-key (kbd "H-i") 'god-local-mode)
  (define-key god-local-mode-map (kbd "i") 'god-local-mode)

  (global-set-key (kbd "C-x C-1") 'delete-other-windows)
  (global-set-key (kbd "C-x C-2") 'split-window-below)
  (global-set-key (kbd "C-x C-3") 'split-window-right)
  (global-set-key (kbd "C-x C-0") 'delete-window))

;; Avy
(defvar my-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))
(use-package avy
  :bind (("C-," . avy-goto-char-2)
	 ("C-'" . avy-goto-char-in-line)
	 ("M-/" . avy-goto-char-timer)
	 ("M-n" . avy-goto-line-below)
	 ("M-p" . avy-goto-line-above))
  :init
  (setq avy-keys my-keys)
  (setq avy-all-windows nil)
  (setq avy-all-windows-alt 'all-frames)
  :config
  (avy-setup-default))

(use-package ace-window
  :bind ("C-o" . ace-window)
  :init
  (setq aw-keys my-keys))

;; Helm
(use-package helm
  :init
  (require 'helm-config)
  :config
  (setq helm-M-x-fuzzy-match 1)
  ;; (setq helm-split-window-in-side-p t)
  (helm-autoresize-mode 1)
  (helm-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  :bind (("C-x C-f" . helm-find-files)
	 ("M-x" . helm-M-x)
	 ("C-x C-b" . helm-mini)))

(use-package yasnippet
  :diminish yas-minor-mode 
  :config
  ;; (setq yas-verbosity 0)
  (push "~/.emacs.d/yasnippet-snippets" yas-snippet-dirs)
  (yas-global-mode 1))

(use-package keyfreq
  :init
  (keyfreq-mode)
  (keyfreq-autosave-mode))

;; Company mode
(use-package company
  :diminish company-mode
  :config
  (setq company-idle-delay 0.1)
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  (global-company-mode))

;; Magit
(use-package magit
  :bind ("<f8>" . magit-status))

(use-package git-gutter+
  :init (global-git-gutter+-mode)
  :config
  (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
  (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
  (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk-inline-at-point)
  (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
  (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
  (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
  (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
  (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
  :diminish (git-gutter+-mode . "gg"))

(use-package git-gutter
  :init
  (global-git-gutter-mode))

;; Projectile
(use-package projectile
  :config
  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm
          projectile-switch-project-action 'helm-projectile)
    (projectile-global-mode)
    (helm-projectile-on)))

;; (use-package whole-line-or-region
;;   :diminish whole-line-or-region-mode
;;   :config
;;   (whole-line-or-region-mode 1))

;; circe
(setq circe-network-options
      `(("Freenode"
         :nick "paldepind"
         :channels ("#emacs" "#haskell"))))

;; Org mode
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  ; (define-key global-map "\C-cl" 'org-store-link) ; Not sure if I want this
  (define-key global-map "\C-ca" 'org-agenda) ; Global hotkey for opening agenda
  (setq org-log-done t) ; Save time when task gets done
  (add-to-list 'org-modules 'org-habit) ; Load the habbits module
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-clock-into-drawer t)
  (setq org-src-fontify-natively t) ; Syntax highlighting for code blocks
  (add-hook 'org-mode-hook 'auto-fill-mode)

  (use-package org-journal
    :init (setq org-journal-dir "~/org/journal/"))

  (use-package org-bullets
    :config (add-hook 'org-mode-hook 'org-bullets-mode 1)))

;; Flycheck
(use-package flycheck
  :config
  (global-flycheck-mode))

;; Elisp
(use-package paren-face
  :config
  (global-paren-face-mode 1))

(use-package smartparens
  :defer t
  :commands (smartparens-mode show-smartparens-mode)
  :config
  (use-package smartparens-config :ensure nil)
  (smartparens-global-strict-mode 1)
  :bind
  (("C-M-k" . sp-kill-sexp)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-p" . sp-backward-down-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-s" . sp-splice-sexp)
   ("M-r" . sp-splice-sexp-killing-around)
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("M-S" . sp-split-sexp)
   ("M-J" . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)))

(use-package nameless
  :commands (nameless-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook 'nameless-mode))

(use-package eldoc
  :diminish eldoc-mode
  :commands eldoc-mode
  :config
  (use-package eldoc-extension
    :disabled t
    :defer t
    :init
    (add-hook 'emacs-lisp-mode-hook
              #'(lambda () (require 'eldoc-extension)) t))
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)

;; Haskell

; (require 'shm)
; (set-face-background 'shm-current-face "#eee8d5")
; (set-face-background 'shm-quarantine-face "lemonchiffon")

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(use-package haskell-mode
  :mode "\\.hs\\'"
  :config
  (require 'haskell-doc)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

  ;; (custom-set-variables
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t)
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-tags-on-save t))


;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook (lambda ()
				  (setq js-indent-level 2
					js2-basic-offset 2)))

;; LaTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; Java
(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 2
				    tab-width 2
				    indent-tabs-mode nil)))

;; Maxima
;;(add-to-list 'load-path "/usr/share/emacs/")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode))

;; Coq
(use-package proof-site
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/ProofGeneral/generic"
  :mode ("\\.v\\'" . coq-mode)
  :config
  (use-package company-coq
    :config
    (add-hook 'coq-mode-hook #'company-coq-mode)))

;; Markdown
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (add-hook 'markdown-mode-hook 'flySpell-mode)
  (add-hook 'markdown-mode-hook 'auto-fill-mode))

(use-package php-mode
  :mode "\\.php\\'")

;; Smart comment

(use-package smart-comment
  :ensure nil
  :commands (smart-comment-region)
  :load-path "~/projects/smart-comment"
  :bind ("M-;" . smart-comment))
