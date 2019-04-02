(when window-system
  (menu-bar-mode -1) ; Disable the menu bar
  (scroll-bar-mode -1) ; Disable the scroll bar
  (tool-bar-mode -1) ; Disable the tool bar
  (tooltip-mode -1)) ; Disable the tooltips

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; use-package install's packages automatically
(setq use-package-always-ensure t)

;; Changes to defaults
;; Nicer scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2) ((control) . nil))) 
(setq mouse-wheel-progressive-speed nil)

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-light-medium t))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/white-paper-theme")
;; (load-theme 'white-paper t)

(set-frame-font "Fira Code")

(set-fontset-font "fontset-default" nil 
                  (font-spec :size 20 :name "Noto Sans Symbols"))

(use-package ripgrep)

(use-package diminish)

(use-package ivy)

(use-package counsel
  :bind ("M-x" . counsel-M-x)
  :commands (counsel-M-x))

(use-package all-the-icons)

(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup))

;; (use-package doom-modeline
;;   :ensure t
;;   :defer t
;;   :hook (after-init . doom-modeline-init))

(use-package which-key)

(use-package evil
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

;; (use-package telephone-line
;;   :config
;;   (telephone-line-mode 1))

(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-mode))

(use-package neotree
  :custom
  (neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(setq-default fill-column 80)

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
    "l" 'helm-imenu
    "w" 'save-buffer))

(use-package prettier-js
  :hook (typescript-mode . prettier-js-mode))

(use-package helm
  :config
  (helm-mode 1)
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  :bind (("C-x C-f" . helm-find-files)
	 ;; ("M-x" . helm-M-x)
         ("C-x C-b" . helm-mini)))

(use-package helm-themes
  :commands (helm-themes))

; (electric-pair-mode)
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

;; Awesome Nyan cat
;; (use-package nyan-mode
;;   :custom
;;   (nyan-wavy-trail t)
;;   :config
;;   (nyan-mode))

(use-package dashboard
  :config
  (setq dashboard-items '((recents . 15)
  			  (bookmarks . 10)
  			  (projects . 20)))
  (dashboard-setup-startup-hook))

(use-package projectile
  :diminish
  :config
  (projectile-mode +1)
  ;; (use-package helm-projectile
  ;;   :config
  ;;   (setq projectile-completion-system 'helm
  ;;         projectile-switch-project-action 'helm-projectile)
  ;;   (helm-projectile-on))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package helm-projectile
  :config
  (setq projectile-completion-system 'helm
	projectile-switch-project-action 'helm-projectile))

(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 1)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-flx
  :after company
  :config
  (company-flx-mode +1))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package magit
  :bind ("<f8>" . magit-status))

(use-package git-gutter
  :diminish
  :init
  (global-git-gutter-mode +1))

(use-package flyspell
  :diminish
  :commands flyspell-mode
  :custom
  (ispell-program-name "aspell")
  :hook (LaTeX-mode . flyspell-mode))

(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . reftex-mode)
	 (LaTeX-mode . prettify-symbols-mode)
	 (LaTeX-mode . (lambda () (define-key LaTeX-mode-map (kbd "$") 'self-insert-command))))
  :custom
  (TeX-PDF-mode t) ; PDF mode instead of DVI
  (TeX-auto-save t)
  (TeX-byte-compile t)
  (TeX-clean-confirm nil)
  (TeX-master 'dwim)
  (TeX-parse-self t)
  (TeX-electric-math t)
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

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-header-scaling t)
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'auto-fill-mode))

;; Scala

(use-package ensime)
(use-package sbt-mode)
(use-package scala-mode)

(use-package json-mode
  :mode "\\.json\\'"
  :custom
  (js-indent-level 2))

;; TypeScript

(use-package typescript-mode
  :mode "\\.ts\\'"
  :custom
  (typescript-indent-level 2))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))

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

(use-package purescript-mode
  :mode "\\.purs\\'"
  :config
  (add-hook 'purescript-mode-hook
	    (lambda ()
	      (turn-on-purescript-indentation))))

(use-package psc-ide
  :hook (purescript-mode . psc-ide-mode))

(use-package indium)

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package multiple-cursors)

;; (use-package keyano
;;   :ensure nil
;;   :load-path "~/projects/keyano/")

;; ** Coq
(use-package proof-general
  :mode ("\\.v\\'" . coq-mode)
  :init (custom-set-variables '(coq-prog-name "~/.opam/default/bin/coqtop") ))

(use-package company-coq
  :config
  (add-hook 'coq-mode-hook #'company-coq-mode))

;; From https://stackoverflow.com/questions/384284/how-do-i-rename-an-open-file-in-emacs
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t t)
 '(TeX-auto-save t t)
 '(TeX-byte-compile t t)
 '(TeX-clean-confirm nil t)
 '(TeX-electric-math t t)
 '(TeX-electric-sub-and-superscript t t)
 '(TeX-master (quote dwim) t)
 '(TeX-parse-self t t)
 '(TeX-source-correlate-mode t t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(company-begin-commands (quote (self-insert-command)))
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(coq-prog-name "~/.opam/default/bin/coqtop")
 '(custom-safe-themes
   (quote
    ("0630e7487d7eec3b83711a70c0055fb63f8630e67476c3ab8b22d83efd9b24fa" "8298b72adbc1f87eb9700f863c675361ea38c1cceccaf0072d2e2b137721da15" "5e7fa06a700480ea1e5d86bec316cc07a009cfeb506e6e051fd014c500c5029b" default)))
 '(fci-rule-color "#171717")
 '(global-company-mode t)
 '(global-font-lock-mode t)
 '(ispell-program-name "aspell")
 '(js-indent-level 2)
 '(neo-theme (quote icons))
 '(nyan-wavy-trail t)
 '(package-selected-packages
   (quote
    (color-theme-sanityinc-solarized avy diminish doom-themes all-the-icons telephone-line helm-rg json-mode smartparens nyan-mode rainbow-delimiters material-theme tao-theme basic-theme spacemacs-theme minimal-theme white-theme tide typescript-mode which-key use-package solarized-theme moody magit helm-themes helm-projectile gruvbox-theme general evil-collection ensime dashboard company-math company-box company-auctex cdlatex)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef")))
 '(prettify-symbols-unprettify-at-point t)
 '(typescript-indent-level 2 t)
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
