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

(defun edit-init ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

;; Nicer scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; * Packages for aesthetics

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one-light t)
  (load-theme 'doom-solarized-light t)
  ;; (setq doom-themes-treemacs-theme "doom-atom")
  ;; (setq doom-themes-treemacs-theme "doom-colors")
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/white-paper-theme")
;; (load-theme 'white-paper t)

;; A selection of fonts
;; (set-frame-font "IBM Plex Mono")
;; (set-frame-font "Fira Code")
(set-frame-font "Source Code Pro")

;; (set-fontset-font "fontset-default" nil
;;                   (font-spec :size 20 :name "Noto Sans Symbols"))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package ripgrep)

;; (use-package diminish)

(use-package ivy
  :config
  (ivy-mode))

(use-package counsel
  :config
  (counsel-mode))

(use-package all-the-icons)

(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup))

(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-mode))

(use-package which-key)

(use-package evil
  :init
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-shift-width 2)
  (setq evil-want-keybinding nil) ; Set to nil per evil-collections documentation
  (setq evil-want-C-u-scroll t)
  (setq evil-disable-insert-state-bindings t) ; This makes it possible to use Emacs bindings in insert mode
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  :config
  (evil-mode 1))

;; * Evil

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :init
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-visual-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  ;; Note that overriding `C-x' in Emacs is probably too crazy.
  (define-key evil-normal-state-map (kbd "M-a") 'evil-numbers/dec-at-pt)
  (define-key evil-visual-state-map (kbd "M-a") 'evil-numbers/dec-at-pt))

(use-package treemacs
  :defer t
  :config
  ;; (treemacs-load-theme "all-the-icons")
  (treemacs-git-mode 'extended)
  (with-eval-after-load 'treemacs
    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)))

(setq-default fill-column 80)

;; This package automatically copies environment variables (in particular PATH)
;; from the shell into Emacs.
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package general
  :config
  (general-evil-setup t)
  (nmap
    :prefix "SPC"
    "f" 'projectile-find-file
    "p" 'projectile-switch-project
    "g" 'magit-status
    "b" 'ivy-switch-buffer
    "m" 'counsel-switch-buffer
    "w" 'save-buffer
    "," 'edit-init))

;; Automatically adding matching braces

;; (electric-pair-mode)
(defun my/newline-indent (&rest _ignored)
  "Insert an extra newline after point, and reindent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (sp-local-pair 'latex-mode "\\[" nil :post-handlers '((my/newline-indent "RET"))))

(use-package dashboard
  :config
  (setq dashboard-items '((recents . 15)
  			  (bookmarks . 10)
  			  (projects . 20)))
  (dashboard-setup-startup-hook))

(use-package projectile
  :diminish
  :init
  (setq projectile-project-search-path '("~/projects/"))
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package company
  :defer 2
  :custom
;   (company-begin-commands '(self-insert-command))
;   (company-idle-delay .1)
;   (company-minimum-prefix-length 1)
;   (company-show-numbers t)
;   (company-tooltip-align-annotations 't)
  (global-company-mode t))

; (use-package flycheck
;   :config
;   (global-flycheck-mode))

(use-package magit
  :commands magit-status)

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

;; Latex

(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . reftex-mode)
	 (LaTeX-mode . prettify-symbols-mode))
  ;; 	 (LaTeX-mode . (lambda () (define-key LaTeX-mode-map (kbd "$") 'self-insert-command))))
  :init
  :custom
  (prettify-symbols-unprettify-at-point t)
  (TeX-save-query nil) ; Save without asking
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  (TeX-view-program-selection '((output-pdf "Skim")))
  (TeX-view-program-list
   '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  ;; (TeX-PDF-mode t) ; PDF mode instead of DVI
  ;; (TeX-auto-save t)
  ;; (TeX-parse-self t)
  ;; (TeX-byte-compile t)
  ;; (TeX-clean-confirm nil)
  ;; (TeX-master 'dwim)
  ;; Insertion of pairs is currently handled by Smartparens
  ;; (LaTeX-electric-left-right-brace t)
  ;; (TeX-electric-math (cons "\\(" "\\)"))
  ;; (TeX-electric-sub-and-superscript t)
  )

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

;; Markdown

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-header-scaling t)
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'auto-fill-mode))

; (use-package yasnippet
;   :diminish yas-minor-mode
;   :config
;   (yas-global-mode 1))

; (use-package multiple-cursors)

;; *  Coq

(use-package proof-general
  :mode ("\\.v\\'" . coq-mode)
  :init (custom-set-variables '(coq-prog-name "~/.opam/default/bin/coqtop")))

(use-package company-coq
  :config
  (add-hook 'coq-mode-hook #'company-coq-mode))


;; Useful function for reaniming the current buffer and file.
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

;; Garbage inserted by Emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coq-prog-name "~/.opam/default/bin/coqtop")
 '(package-selected-packages
   '(evil-numbers evil-surround which-key use-package treemacs-all-the-icons smartparens ripgrep rainbow-delimiters proof-general projectile neotree markdown-mode magit highlight-indent-guides gruvbox-theme git-gutter general exec-path-from-shell evil-commentary evil-collection doom-themes doom-modeline diminish dashboard counsel company-coq company-auctex all-the-icons-ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
