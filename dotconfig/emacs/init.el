;; -*- lexical-binding: t; -*-

(defun start/display-startup-time ()
  "Display Emacs startup time and number of garbage collections."
  (interactive)
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

;; Display startup time, this can be handy to keep an eye on things.
(add-hook 'emacs-startup-hook #'start/display-startup-time)

;; -----------------------------------------------------------------------------
;; Config of built-in settings
;; -----------------------------------------------------------------------------

(pixel-scroll-precision-mode) ; Pixel-wise scrolling
(setq frame-resize-pixelwise t) ; Pixel-wise resizing

(setq-default fill-column 80)

;; Save minibuffer history: future invocations will have recently-used
;; selections sorted first.
(savehist-mode)

(add-to-list
 'default-frame-alist '(font . "Codelia Ligatures-10:weight=medium"))

(load-theme 'modus-operandi)
;; (load-theme 'modus-vivendi) ;; dark theme

;; Add the MELPA repository
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package project
  :custom
  (project-vc-extra-root-markers '("Cargo.toml" "package.json"))
  (project-mode-line t)) ; show project name in mode line

;; -----------------------------------------------------------------------------
;; UI/eye-candy
;; -----------------------------------------------------------------------------

(use-package nerd-icons
  :ensure t
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; This assumes you've installed the package via MELPA.
(use-package ligature
  :ensure t
  :config
  ;; Enable all Cascadia Code ligatures in programming modes
  ;; format-next-line: off
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers.
  (global-ligature-mode t))

(use-package ace-window
  :ensure t
  :commands ace-window
  :custom (aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))

;; Completion things

;; Fuzzier completion
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Enable Vertico.
(use-package vertico
  :ensure t
  :custom
  (vertico-count 20) ;; Show more candidates
  :init (vertico-mode))

(use-package consult
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :config
  (setopt magit-format-file-function #'magit-format-file-nerd-icons) ;; Turns on nerd-icons
  :defer t)

(use-package ghostel
  :ensure t)

;; -----------------------------------------------------------------------------
;; Evil

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode 1))

(use-package evil
  :ensure t
  :custom
  (evil-want-Y-yank-to-eol t) ; `Y` yanks to end of line
  (evil-shift-width 2) ; Shifting operators (`<` and `>`) shift by two spaces
  (evil-symbol-word-search t) ; `*` and `#` search for symbols, not words
  (evil-want-keybinding nil) ; Set to nil per evil-collections documentation
  (evil-want-C-u-scroll t) ; `C-u` scrolls up
  (evil-disable-insert-state-bindings t) ; This makes it possible to use Emacs bindings in insert mode
  (evil-undo-system 'undo-tree)
  :config
  (evil-mode)
  (add-hook 'git-commit-setup-hook 'evil-insert-state) ; Committing in magit starts in insert mode
  )

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (nmap
   :prefix
   "SPC"
   "q"
   'delete-window
   "RET"
   'ghostel
   ;; "p"
   ;; 'projectile-find-file
   ;; "P"
   ;; 'projectile-switch-project
   "g"
   'magit-status
   ;; switch buffer
   "m"
   'consult-buffer
   "w"
   'save-buffer
   ","
   'my/init-file
   "l"
   'consult-line
   "/"
   'consult-ripgrep
   ;; "k"
   ;; 'avy-goto-line-above
   ;; "j"
   ;; 'avy-goto-line-below
   "o"
   'ace-window))

;; -----------------------------------------------------------------------------
;; Programming

(use-package eglot
  :commands (eglot)
  :hook (before-save . eglot-format))

;; -----------------------------------------------------------------------------
;; Elisp

(use-package elisp-autofmt
  :ensure t
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

;; -----------------------------------------------------------------------------
;; Rust

(use-package rust-mode
  :hook (rust-mode . (lambda () (setq indent-tabs-mode nil)))
  :ensure t)

;; -----------------------------------------------------------------------------
;; Writing

;; Spell checking based on enchant <https://rrthomas.github.io/enchant>
(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :config (push 'yaml-mode jinx-camel-modes)
  :bind (("M-$" . jinx-correct) ("C-M-$" . jinx-languages)))

;; Minor mode that automatically balance window margins to center text
(use-package olivetti
  :custom (olivetti-style 'fancy)
  ;; Make `olivetti-mode` turn on automatically in certain modes
  :hook ((markdown-ts-mode . olivetti-mode) (markdown-mode . olivetti-mode)))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-header-scaling t)
  (markdown-enable-math t)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map ("C-c C-e" . markdown-do)))

;; -----------------------------------------------------------------------------
;; AI stuff

(use-package agent-shell
  :ensure t)

;; -----------------------------------------------------------------------------
;; My custom functions

(defun my/init-file ()
  "Open init.org configuration file."
  (interactive)
  (find-file "~/.config/emacs/init.el"))

(defun my/early-init-file ()
  "Open early-init.org configuration file."
  (interactive)
  (find-file "~/.config/emacs/early-init.el"))

(defun my/reload-init ()
  "Reload init.el."
  (interactive)
  (load-file "~/.config/emacs/init.el"))

;; -----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(0blayout
     ace-window
     agent-shell
     consult
     elisp-autofmt
     evil-collection
     general
     ghostel
     jinx
     ligature
     magit
     nerd-icons
     olivetti
     orderless
     rust-mode
     undo-tree
     vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Local variables:
;; elisp-autofmt-load-packages-local: ("use-package" "use-package-core")
;; end:
