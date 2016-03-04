(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

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

(setq confirm-kill-emacs 'y-or-n-p)

(delete-selection-mode 1) ;; typed text replaces active selection-coding-system
(setq-default indent-tabs-mode nil) ;; use spaces for indentation

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "NIX_GHC")
(exec-path-from-shell-copy-env "NIX_GHCPKG")
(exec-path-from-shell-copy-env "NIX_GHC_DOCDIR")
(exec-path-from-shell-copy-env "NIX_GHC_LIBDIR")

;; scroll one line at a time (less "jumpy" than defaults)
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

;; animated scrolling
(use-package sublimity
  :init
  (require 'sublimity-scroll)
  :config
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
(setq color-themes '())
(use-package color-theme-solarized
  :config
  (load-theme 'solarized t))

(use-package smart-mode-line
  :config
  (setq sml/theme 'respectful
	sml/no-confirm-load-theme t
	sml/shorten-directory t
	sml/name-width '(32 . 48)
	sml/shorten-modes t
	sml/use-projectile-p 'before-prefixes
	sml/projectile-replacement-format "[%s]")
  (sml/setup))


(defun my-resize-margins ()
  (let ((margin-size (/ (- (frame-width) 80) 2)))
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

;; god-mode
(define-key input-decode-map (kbd "C-i") (kbd "H-i")) ; Allow binding to C-i as H-i
(use-package god-mode
  :config
  (global-set-key (kbd "H-i") 'god-local-mode)
  (define-key god-local-mode-map (kbd "i") 'god-local-mode)

  (global-set-key (kbd "C-x C-1") 'delete-other-windows)
  (global-set-key (kbd "C-x C-2") 'split-window-below)
  (global-set-key (kbd "C-x C-3") 'split-window-right)
  (global-set-key (kbd "C-x C-0") 'delete-window)
)

;; Avy
(use-package avy
  :bind (("C-," . avy-goto-char-2)
	 ("C-'" . avy-goto-char-in-line)
	 ("M-/" . avy-goto-char-timer)
	 ("M-n" . avy-goto-line-below)
	 ("M-p" . avy-goto-line-above))
  :init
  (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))
  (setq avy-all-windows nil)
  (setq avy-all-windows-alt 'all-frames)
  :config
  (avy-setup-default)
)

(use-package ace-window
  :bind ("C-o" . ace-window)
  :init
  (setq aw-keys '(?a ?r ?s ?t ?d ?h ?e ?v ?k))
  (setq aw-dispatch-always t)
)

;; Helm
(use-package helm
  :init
  (require 'helm-config)
  (helm-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  :bind (("C-x C-f" . helm-find-files)
	 ("M-x" . helm-M-x)
	 ("C-x C-b" . helm-mini)))

;; Company mode
(use-package company
  :diminish company-mode
  :config
  (global-company-mode))

;; Projectile
(use-package projectile
  :config
  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm
          projectile-switch-project-action 'helm-projectile)
    (projectile-global-mode)
    (helm-projectile-on)))

(use-package whole-line-or-region
  :diminish whole-line-or-region-mode
  :config
  (whole-line-or-region-mode 1))

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
  (auto-fill-mode)

  (use-package org-journal
    :init (setq org-journal-dir "~/org/journal/"))

  (use-package org-bullets
    :config (add-hook 'org-mode-hook 'org-bullets-mode 1)))

;; Haskell

;; (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  ;; (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  ;; (add-to-list 'exec-path my-cabal-path))

; (require 'shm)
; (set-face-background 'shm-current-face "#eee8d5")
; (set-face-background 'shm-quarantine-face "lemonchiffon")

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(use-package haskell-mode
  :mode "\\.hs\\'"
  :config
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  ;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  ;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

  (custom-set-variables
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-tags-on-save t))
  
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "C-c C-t") 'ghc-show-type)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  
  (add-to-list 'company-backends 'company-ghc))


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
  :config (add-hook 'markdown-mode-hook 'flyspell-mode))
