;;; init.el --- My init file -*- lexical-binding: t -*-

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq-default scroll-bar-width 6)
(set-scroll-bar-mode 'right)
(setq-default cursor-type 'bar)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
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

(delete-selection-mode 1) ;; typed text replaces active selection-coding-system
(setq-default indent-tabs-mode nil) ;; use spaces for indentation
(show-paren-mode 1)
(subword-mode 1)

(global-set-key (kbd "<f9>") 'previous-buffer)
(global-set-key (kbd "<f10>") 'next-buffer)
(bind-keys
 ("C-l" . imenu))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)

;; scroll one line at a time (less "jumpy" than defaults)
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

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

(setq color-themes '())

(defun disable-all-themes ()
  "Disable all activated color-themes."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun set-theme (theme)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (disable-all-themes)
  (load-theme theme t))

(load-theme 'zenburn t)
;; (use-package color-theme-solarized
;;   :config
;;   (load-theme 'solarized t))

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

;; Let's us bind suff to C-m
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
  (global-set-key (kbd "C-x C-0") 'delete-window)

  
  (defun god-toggle-on-overwrite ()
    "Toggle god-mode on overwrite-mode."
    (if (bound-and-true-p overwrite-mode)
        (god-local-mode-pause)
      (god-local-mode-resume)))

  (add-hook 'composable-object-mode-hook 'god-toggle-on-overwrite))

;; Avy
(use-package avy
  :bind (("C-," . avy-goto-char-2)
	 ("C-'" . avy-goto-char-in-line)
	 ("M-/" . avy-goto-char-timer)
	 ("M-n" . avy-goto-line-below)
	 ("M-p" . avy-goto-line-above)
         ("M-a" . avy-goto-word-1-above)
         ("M-e" . avy-goto-word-1-below)
         ("M-i" . avy-goto-line-above))
  :init
  (defvar my-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o ?k ?v ?m ?c))
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
  (setq company-idle-delay 0.2)
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

;; circe
(setq circe-network-options
      `(("Freenode"
         :nick "paldepind"
         :channels ("#emacs" "#haskell"))))

;; Org mode
(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :config
  (define-key global-map "\C-ca" 'org-agenda) ; Global hotkey for opening agenda
  (add-to-list 'org-modules 'org-habit) ; Load the habbits module
  (add-to-list 'org-modules 'org-drill) ; Load the drill module
  (org-clock-persistence-insinuate)

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (setq org-log-done t) ; Save time when task gets done
  (setq org-clock-persist 'history)
  (setq org-clock-into-drawer t)
  (setq org-src-fontify-natively t) ; Syntax highlighting for code blocks
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (use-package cdlatex
    :init
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex))
  (setq org-highlight-latex-and-related '(latex script entities))
  (add-hook 'org-mode-hook 'flyspell-mode)

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

(electric-pair-mode)
(use-package smartparens
  :defer t
  ;; :commands (smartparens-mode show-smartparens-mode)
  :config
  (use-package smartparens-config :ensure nil)
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

(use-package scheme-mode
  :ensure nil
  :init
  (setq scheme-program-name "petite")
  :mode "\\.scm\\'"
  :config
  (add-hook 'inferior-scheme-mode-hook
            (lambda ()
              ;; Overwrite the standard 'switch-to-buffer' to use
              ;; 'switch-to-buffer-other-window'
              (defun switch-to-scheme (eob-p)
                "Switch to the scheme process buffer.
     With argument, position cursor at end of buffer."
                (interactive "P")
                (if (or (and scheme-buffer (get-buffer scheme-buffer))
                        (scheme-interactively-start-process))
                    (switch-to-buffer-other-window scheme-buffer)
                  (error "No current process buffer.  See variable `scheme-buffer'"))
                (when eob-p
                  (push-mark)
                  (goto-char (point-max)))))))

;; (atuoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
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

  (use-package intero
    :init
    (add-hook 'haskell-mode-hook 'intero-mode))
  ;; (autoload 'ghc-init "ghc" nil t)
  ;; (autoload 'ghc-debug "ghc" nil t)
  ;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

  ;; (custom-set-variables
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t)
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-tags-on-save t))


;; JavaScript
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :bind (("C-c n" . js2-next-error)
         :map js2-mode-map
         ("M-j" . nil))
  :config
  (setq js2-basic-offset 2
        js2-bounce-indent-p t
        js-indent-level 2)
  ;; add tern IDE features in JavaScript
  (use-package tern
    :config
    (add-hook 'js-mode-hook 'tern-mode)
    ;; autocompletion
    (use-package company-tern
      :config (add-to-list 'company-backends 'company-tern)))
  ;; Lots of handy refactor functions
  (use-package js2-refactor
    :diminish ""
    :bind (:map js2-mode-map
                ("C-)" . js2r-forward-slurp)
                ("M-)" . js2r-forward-barf)
                ("M-]" . js2r-unwrap))
    :config
    (js2r-add-keybindings-with-prefix "C-c C-m")
    (add-hook 'js2-mode-hook #'js2-refactor-mode)))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook
            (lambda ()
              (tide-setup)
              (eldoc-mode +1))))

(use-package web-mode
  :mode "\\.tsx\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (tide-setup)
                (flycheck-mode +1)
                (setq flycheck-check-syntax-automatically '(save mode-enabled))
                (eldoc-mode +1)
                (company-mode-on)))))

(use-package tide
  :commands (tide-setup))

(use-package tex-site
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode))

;; Java
(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 4
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
  (setq markdown-header-scaling t)
  ;; (custom-set-faces
  ;;  '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
  ;;  '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.8))))
  ;;  '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.4))))
  ;;  '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2)))))
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'auto-fill-mode))

(use-package php-mode
  :mode "\\.php\\'")

;; Smart comment
(use-package smart-comment
  :ensure nil
  :commands (smart-comment-region)
  :load-path "~/projects/smart-comment"
  :bind ("M-;" . smart-comment))

(use-package flyspell 
  :bind ("<f7>" . flyspell-switch-dictionary)
  :config
  (setq ispell-program-name "aspell"
        ispell-dictionary "english")
  (add-hook 'flyspell-mode-hook 'flyspell-buffer)
  (defun flyspell-switch-dictionary()
    (interactive)
    (let* ((dic ispell-current-dictionary)
           (change (if (string= dic "dansk") "english" "dansk")))
      (ispell-change-dictionary change)
      (message "Dictionary switched from %s to %s" dic change))))
