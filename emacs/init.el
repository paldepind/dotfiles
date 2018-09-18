(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")))

; use-package install's packages automatically
(setq use-package-always-ensure t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-light-medium t))

(set-default-font "IBM Plex Mono")

(use-package which-key)

(use-package evil
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(setq-default fill-column 80)

(when window-system
  (menu-bar-mode -1) ; Disable the menu bar
  (scroll-bar-mode -1) ; Disable the scroll bar
  (tool-bar-mode -1) ; Disable the tool bar
  (tooltip-mode -1)) ; Disable the tooltips

;; This package automatically copies environment variables (in particular PATH)
;; from the shell into Emacs.
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package general :ensure t
  :config
  (general-evil-setup t)
  (nmap
    :prefix "SPC"
    "m" 'helm-mini
    "w" 'save-buffer))

(use-package helm
  :config
  (helm-mode 1)
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  :bind (("C-x C-f" . helm-find-files)
	 ("M-x" . helm-M-x)
         ("C-x C-b" . helm-mini)))

(use-package helm-themes
  :commands (helm-themes)
  )

(add-to-list 'custom-theme-load-path "~/projects/white-paper-theme")

;; Awesome Nyan cat
(use-package nyan-mode
  :custom
  (nyan-wavy-trail t)
  :config
  (nyan-mode))

(use-package dashboard
  :config
  (setq dashboard-items '((recents . 15)
  			  (bookmarks . 10)
  			  (projects . 20)))
  (dashboard-setup-startup-hook))

(use-package projectile
  :config
  (projectile-mode +1)
  ;; (use-package helm-projectile
  ;;   :config
  ;;   (setq projectile-completion-system 'helm
  ;;         projectile-switch-project-action 'helm-projectile)
  ;;   (helm-projectile-on))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package magit
  :bind ("<f8>" . magit-status))

(use-package git-gutter
  :init
  (global-git-gutter-mode +1))

(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . reftex-mode)
	 (LaTeX-mode . prettify-symbols-mode))
  :custom
  (TeX-PDF-mode t) ; PDF mode instead of DVI
  (TeX-auto-save t)
  (TeX-byte-compile t)
  (TeX-clean-confirm nil)
  (TeX-master 'dwim)
  (TeX-parse-self t)
  (TeX-electric-sub-and-superscript t)
  (TeX-source-correlate-mode t)
  (prettify-symbols-unprettify-at-point t))

(use-package bibtex
  :after auctex
  :hook (bibtex-mode . my/bibtex-fill-column)
  :preface
  (defun my/bibtex-fill-column ()
    "Ensures that each entry does not exceed 120 characters."
    (setq fill-column 120)))

(use-package company-auctex
  :after (auctex company)
  :config (company-auctex-init))

(use-package company-math :after (auctex company))

;; Scala

(use-package ensime)
(use-package sbt-mode)
(use-package scala-mode)

;; TypeScript

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))

(use-package indium)

(use-package yasnippet
  :diminish
  :config
  (yas-global-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-auto-save t)
 '(TeX-byte-compile t)
 '(TeX-clean-confirm nil)
 '(TeX-electric-sub-and-superscript t)
 '(TeX-master (quote dwim))
 '(TeX-parse-self t)
 '(TeX-source-correlate-mode t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(company-begin-commands (quote (self-insert-command)))
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(fci-rule-color "#171717")
 '(global-company-mode t)
 '(global-font-lock-mode t)
 '(package-selected-packages
   (quote
    (nyan-mode rainbow-delimiters material-theme tao-theme basic-theme spacemacs-theme minimal-theme white-theme tide typescript-mode which-key use-package solarized-theme moody magit helm-themes helm-projectile gruvbox-theme general evil-collection ensime dashboard company-math company-box company-auctex cdlatex)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef")))
 '(prettify-symbols-unprettify-at-point t)
 '(vc-annotate-background "#0E0E0E")
 '(vc-annotate-color-map
   (quote
    ((20 . "#616161")
     (40 . "#9E9E9E")
     (60 . "#9E9E9E")
     (80 . "#C3C3C3")
     (100 . "#C3C3C3")
     (120 . "#DADADA")
     (140 . "#DADADA")
     (160 . "#E8E8E8")
     (180 . "#E8E8E8")
     (200 . "#E8E8E8")
     (220 . "#F1F1F1")
     (240 . "#F1F1F1")
     (260 . "#F1F1F1")
     (280 . "#F6F6F6")
     (300 . "#F6F6F6")
     (320 . "#F6F6F6")
     (340 . "#FAFAFA")
     (360 . "#FAFAFA"))))
 '(vc-annotate-very-old-color "#DADADA"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
