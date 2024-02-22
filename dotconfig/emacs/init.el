(when window-system
  (menu-bar-mode 1) ; Disable the menu bar
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

;; Nicer scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1)
;; (pixel-scroll-precision-mode)

;; Disable beeping
(setq ring-bell-function 'ignore)

(setq window-divider-default-right-width 100)

(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
(global-set-key (kbd "s-,") 'edit-init)
(setq frame-resize-pixelwise t)
(setq-default word-wrap t)

;; * Packages for aesthetics

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'doom-one-light t))
    ('dark (load-theme 'doom-one t))))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
  ()
  ;; (setq doom-themes-treemacs-theme "doom-atom")
  ;; (setq doom-themes-treemacs-theme "doom-colors")
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/white-paper-theme")
;; (load-theme 'white-paper t)

;; ;; A selection of fonts
;; (set-face-attribute 'default nil
;;                     ;; :family "Source Code Pro"
;;                     ;; :family "IBM Plex Mono"
;;                     ;; :family "Fira Code"
;;                     ;; :family "JetBrains Mono"
;;                     :family "Fantasque Sans Mono" :height 160
;;                     ;; :family "Cascadia Code" :height 135
;;                     ;; :family "Menlo"
;;                     ;; :family "Monaco"
;;                     ;; :family "Roboto Mono"
;;                     ;; :height 140
;;                     :weight 'normal
;;                     :width 'normal)

(set-face-attribute 'default nil
                    :family "Comic Code Ligatures" :height 120
                    :weight 'medium)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package idle-highlight-mode
  :config (setq idle-highlight-idle-time 0.2)
  :hook ((prog-mode text-mode) . idle-highlight-mode))

(use-package default-text-scale)

(use-package ripgrep)

;; (use-package diminish)

(use-package avy
  :custom
  (avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))

(use-package ace-window
  :commands ace-window
  :custom
  (aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))

(use-package smex)

(use-package ivy
  :custom
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (ivy-height 25)
  :config
  (ivy-mode))

;; (use-package ivy-posframe
;;   :init
;;   (setq ivy-posframe-display-functions-alist
;; 	'((swiper            . ivy-display-function-fallback)
;; 	  (t                 . ivy-posframe-display)))
;; 	  ;; (t                . ivy-display-function-fallback)))
;;   :custom
;;   (ivy-posframe-border-width 28)
;;   :config
;;   (ivy-posframe-mode 1))

(use-package ivy-avy
  :after ivy)

(use-package counsel
  :config
  (counsel-mode))

(use-package all-the-icons)

(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup))

(use-package vterm
  :ensure t
  :commands vterm)

(use-package vterm-toggle
  :ensure t
  :commands vterm-toggle)

(use-package which-key
  :config
  (which-key-mode))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

(use-package evil
  :init
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-shift-width 2)
  (setq evil-symbol-word-search t)
  (setq evil-want-keybinding nil) ; Set to nil per evil-collections documentation
  (setq evil-want-C-u-scroll t)
  (setq evil-disable-insert-state-bindings t) ; This makes it possible to use Emacs bindings in insert mode
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  :custom
  (evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))

;; * Evil

(use-package evil-collection
  :after evil
  :config
  (evil-collection-define-key 'normal 'coq-mode-map
    ;; goto
    "gd" 'company-coq-jump-to-definition)
  (evil-collection-define-key 'normal 'coq-goals-mode-map
    ;; goto
    "gd" 'company-coq-jump-to-definition)
  (evil-collection-define-key 'normal 'coq-response-mode-map
    ;; goto
    "gd" 'company-coq-jump-to-definition)
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
  ;; Single-click instead of double-click
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  ;; Hide gitignored files
  (treemacs-git-mode 'extended)
  (with-eval-after-load 'treemacs
    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))
  :bind
  (:map global-map
	;; VSCode style binding
	("s-E" . treemacs-display-current-project-exclusively)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

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
    "q" 'delete-window
    "RET" 'vterm-toggle
    "p" 'projectile-find-file
    "P" 'projectile-switch-project
    "g" 'magit-status
    "b" 'ivy-switch-buffer
    "m" 'counsel-switch-buffer
    "w" 'save-buffer
    "," 'edit-init
    "l" 'swiper
    "/" 'counsel-rg
    "k" 'avy-goto-line-above
    "j" 'avy-goto-line-below
    "o" 'ace-window))

;; Automatically adding matching braces

;; * Packages for general editing

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; (electric-pair-mode)
(defun my/newline-indent (&rest _ignored)
  "Insert an extra newline after point, and reindent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;; (use-package smartparens
;;   :config
;;   (require 'smartparens-config)
;;   (smartparens-global-mode t)
;;   (show-smartparens-global-mode t)
;;   (sp-local-pair 'latex-mode "\\[" nil :post-handlers '((my/newline-indent "RET")))
;;   (sp-with-modes '(coq-mode)
;;     ;; Disable ` because it is used in implicit generalization
;;     (sp-local-pair "`" nil :actions nil)
;;     ;; Disable ' because it is used in pattern-matching
;;     (sp-local-pair "'" nil :actions nil)
;;     (sp-local-pair "(*" "*)" :actions nil)
;;     (sp-local-pair "(*" "*"
;; 		    :actions '(insert)
;; 		    :post-handlers '(("| " "SPC") ("|\n[i]*)[d-2]" "RET")))))

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

(use-package flycheck)
  ;; :config
  ;; (global-flycheck-mode))

(use-package magit
  ;; :config
  ;; (transient-append-suffix 'magit-pull "-r"
  ;;   '("-a" "Autostash" "--autostash"))
  :commands magit-status
  :custom
  ;; Makes Magit windows use the full _height_ of the frame
  (magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1))

;; (use-package forge
;;   :after magit)

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

;; lsp-mode

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  ;; :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
  ;;        (XXX-mode . lsp)
  ;;        ;; if you want which-key integration
  ;;        (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; Latex

(use-package tex
  :ensure auctex
  :config
  ;; (define-key LaTeX-mode-map (kbd "M-q") ')
  ;; :bind
  ;; ([remap fill-paragraph] . fill-paragraph-one-sentence-per-line)
  :hook
  ((LaTeX-mode . reftex-mode)
   (LaTeX-mode . prettify-symbols-mode)
   (LaTeX-mode . (lambda ()
		   (LaTeX-add-environments
		    '("mathpar" LaTeX-env-label)))))
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

(use-package biblio)

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

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package multiple-cursors)

;; *  Coq
;; Moves subscript _a to \_a

(defvar sm-quail-activate-hook-done nil)
(defun sm-quail-activate-hook ()
  (unless (member (quail-name) sm-quail-activate-hook-done)
    (push (quail-name) sm-quail-activate-hook-done)
    (when (member (quail-name) '("TeX"))
      ;; Copy the "_..." bindings to "\_...".
      (setf (alist-get ?_ (cdr (alist-get ?\\ (quail-map))))
            (alist-get ?_ (quail-map)))
      ;; Remove the "_..." bindings.
      (setf (alist-get ?_ (quail-map)) nil)
      (let ((quail-current-package (assoc "TeX" quail-package-alist)))
        (quail-define-rules ((append . t))
                            ("^\\alpha" ?ᵅ)
                            ("\\Phi"    ?Φ)
                            ("\\fun"    ?λ)
                            ("\\mult"   ?⋅)
                            ("\\ent"    ?⊢)
                            ("\\valid"  ?✓)
                            ("\\diamond" ?◇)
                            ("\\box"    ?□)
                            ("\\bbox"   ?■)
                            ("\\later"  ?▷)
                            ("\\unit"   ?ε)
                            ("\\pred"   ?φ)
                            ("\\and"    ?∧)
                            ("\\or"     ?∨)
                            ("\\comp"   ?∘)
                            ("\\ccomp"  ?◎)
                            ("\\all"    ?∀)
                            ("\\ex"     ?∃)
                            ("\\to"     ?→)
                            ("\\sep"    ?∗)
                            ("\\colon"  ?∷)
                            ("\\col"    ?∷)
                            ("\\lc"     ?⌜)
                            ("\\rc"     ?⌝)
                            ("\\Lc"     ?⎡)
                            ("\\Rc"     ?⎤)
                            ("\\lam"    ?λ)
                            ("\\empty"  ?∅)
                            ("\\Lam"    ?Λ)
                            ("\\Sig"    ?Σ)
                            ("\\sig"    ?σ)
                            ("\\-"      ?∖)
                            ("\\aa"     ?●)
                            ("\\af"     ?◯)
                            ("\\auth"   ?●)
                            ("\\frag"   ?◯)
                            ("\\iff"    ?↔)
                            ("\\gname"  ?γ)
                            ("\\incl"   ?≼)
                            ("\\latert" ?▶)
                            ("\\light" ?⚡)
                            ("\\update" ?⇝))))))
(add-hook 'quail-activate-hook #'sm-quail-activate-hook)

(setq coq-smie-user-tokens
      '(("∗" . "*")
	("-∗" . "->")
	("∗-∗" . "<->")
	("==∗" . "->")
	("⊢" . "->")
	("⊣⊢" . "<->")
	("⋅" . "*")
	(":>" . ":=")
	("by" . "now")
	("forall" . "now")))

(use-package proof-general
  :mode ("\\.v\\'" . coq-mode)
  :custom
  (proof-three-window-mode-policy 'hybrid)
  (proof-splash-enable nil)
  (coq-compile-before-require t)
  (coq-compile-vos 'vos-and-vok)
  (proof-omit-proofs-option t)
  ;; (coq-diffs 'on)
  (proof-script-fly-past-comments t)
  ;; (add-hook 'coq-mode-hook (lambda () (undo-tree-mode 1)))
  ;; (coq-prog-name "~/.opam/default/bin/coqtop")
  :init
  (setq coq-smie-user-tokens
	'(("∗" . "*")
	  ("-∗" . "->")
	  ("∗-∗" . "<->")
	  ("==∗" . "->")
	  ("⊢" . "->")
	  ("⊣⊢" . "<->")
	  ("⋅" . "*")
	  (":>" . ":=")
	  ("by" . "now")
	  ("forall" . "now")))
  (add-hook 'coq-mode-hook (lambda ()
			     (undo-tree-mode 1)
			     (set-input-method "TeX"))))

(use-package company-coq
  :config
  ;; (add-hook 'coq-mode-hook #'company-coq-mode)
  :custom
  (company-coq-disabled-features '(prettify-symbols))
  (company-coq-live-on-the-edge t))

;; * Custom functions

(defun edit-init ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

;; Useful function for renaming the current file and buffer.
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

(defun fill-paragraph-one-sentence-per-line (&optional P)
  "When called with prefix argument call `fill-paragraph'.
Otherwise split the current paragraph into one sentence per line."
  (interactive "P")
  (if (not P)
      (save-excursion
        (let ((fill-column 12345678)) ;; relies on dynamic binding
          (fill-paragraph) ;; this will not work correctly if the paragraph is
                           ;; longer than 12345678 characters (in which case the
                           ;; file must be at least 12MB long. This is unlikely.)
          (let ((end (save-excursion
                       (forward-paragraph 1)
                       (backward-sentence)
                       (point-marker))))  ;; remember where to stop
            (beginning-of-line)
            (while (progn (forward-sentence)
                          (<= (point) (marker-position end)))
              (just-one-space) ;; leaves only one space, point is after it
              (delete-char -1) ;; delete the space
              (newline)        ;; and insert a newline
              (LaTeX-indent-line) ;; I only use this in combination with late, so this makes sense
              ))))
    ;; otherwise do ordinary fill paragraph
    (fill-paragraph P)))

;; (use-package rustic)

;; Garbage inserted by Emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "e8882a809fb14f2307410a659846a06bfa58a2279ffb1f5aca0c3aecbcb6aaee" default))
 '(package-selected-packages
   '(default-text-scale multiple-cursors ws-butler which-key wgrep vterm-toggle use-package undo-tree treemacs-projectile treemacs-evil treemacs-all-the-icons smex smartparens rustic ripgrep rainbow-delimiters proof-general neotree lsp-ui lsp-ivy ivy-posframe ivy-avy idle-highlight-mode idle-highlight-in-visible-buffers-mode highlight-indent-guides gruvbox-theme good-scroll git-gutter general forge flycheck fast-scroll exec-path-from-shell evil-surround evil-numbers evil-commentary evil-collection doom-themes doom-modeline diminish dashboard dap-mode counsel-projectile company-posframe company-coq company-auctex biblio almost-mono-themes all-the-icons-ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
