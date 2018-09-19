;;; white-paper-theme.el

;; Copyright (C) 2018 Simon Friis Vindum

;; Author: Simon Friis Vindum
;; Keywords: themes
;; URL: http://github.com/paldepind/white-paper-theme
;; Version: 2018
;; X-Original-Version: 0.1
;; Package-Requires: ((emacs "24.1"))

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(deftheme white-paper "White mostly greyscale color theme")

(display-color-cells (selected-frame))
(let* ((class '((class color) (min-colors 89)))
       (256color (eq (display-color-cells (selected-frame)) 256))
       (truecolor (eq (display-color-cells (selected-frame)) 16777216))

       (foreground "#111111")
       (background "#FFFCFC") ;; sidebar-container
       (current-line (if (or window-system truecolor)  "#ECEFF1" "#dadada"))
       (far-background (if (or window-system truecolor)  "#e0f7fa" "#e0f7fa"))
       (mode-line-background "#dee1e6")
       (inactive-gray "#cfd8dc")
       (header-color "#C8E6C9")
       (dark "#4a4a4a")
       (faded "#9a9a9a") ;; Used for text that is less important
       (subtle "#a7adba") ;; tree-row-hover-disclosure-button-control
       (selection "#90A4AE")
       (secondary-selection "#bf616a") ;; tab-control-hover-tab-close-button
       (comment "#607d8b") ;; table-row
       (red "#B71C1C") ;; tab-control-hover-tab-close-button
       (orange "#FF5722") ;; darker tab-control-dirty-tab-close-butto
       (yellow "#FFA000") ;; tab-control-dirty-tab-close-button
       (green "#4bb75b")
       (aqua "#00796b") ;; lighter complement tab-control-dirty-tab-close-button
       (blue "#2196f3") ;; complement tab-control-dirty-tab-close-button
       (purple "#4527A0")) ;; complement tab-control-dirty-tab-close-button

  (custom-theme-set-faces
   'white-paper
   `(default ((,class (:foreground ,foreground :background ,background))))
   `(bold ((,class (:weight bold))))
   `(bold-italic ((,class (:slant italic :weight bold))))
   `(underline ((,class (:underline t))))
   `(italic ((,class (:slant italic))))
   `(font-lock-builtin-face ((,class (:foreground ,red))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment))))
   `(font-lock-comment-face ((,class (:foreground ,comment, :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,green))))
   `(font-lock-doc-face ((,class (:foreground "#673ab7"))))
   `(font-lock-doc-string-face ((,class (:foreground ,yellow))))
   `(font-lock-function-name-face ((,class (:foreground ,"#0097A7"))))
   `(font-lock-keyword-face ((,class (:foreground ,faded))))
   `(font-lock-negation-char-face ((,class (:foreground ,blue))))
   `(font-lock-preprocessor-face ((,class (:foreground ,yellow))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,purple))))
   `(font-lock-string-face ((,class (:foreground ,foreground))))
   `(font-lock-type-face ((,class (:foreground "#0097A7"))))
   `(font-lock-variable-name-face ((,class (:foreground ,"#EF6C00"))))
   `(font-lock-warning-face ((,class (:weight bold :foreground ,red))))
   `(highlight-numbers-number ((,class (:foreground ,"#689f38"))))
   `(shadow ((,class (:foreground ,comment))))
   `(success ((,class (:foreground "SeaGreen2"))))
   `(error ((,class (:foreground ,red))))
   `(warning ((,class (:foreground ,orange))))

   ;; ace-window faces
   `(aw-leading-char-face ((,class (:foreground ,
                                    background
                                    :background ,
                                    green
                                    :height ,
                                    1.7
                                    :weight
                                    bold))))

   ;; ace-jump-faces
   `(ace-jump-face-foreground ((,class (:foreground ,background
                                        :background ,green
                                        :weight bold))))

   `(ace-jump-face-background ((,class (:foreground ,inactive-gray
                                        :weight normal))))

   ;; ace-jump-mode
   `(avy-background-face ((,class (:foreground ,inactive-gray
                                               :weight normal))))
   `(avy-lead-face-0 ((,class (:foreground ,background
                                        :background ,green
                                        :weight bold))))
   `(avy-lead-face ((,class (:foreground ,background
                                        :background ,green
                                        :weight bold))))

   ;; Flycheck
   `(flycheck-error ((,class (:underline (:style wave :color ,red)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,blue)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,orange)))))
   `(flycheck-fringe-error ((,class (:foreground ,red :background ,current-line))))
   `(flycheck-fringe-info ((,class (:foreground ,blue :background ,current-line))))
   `(flycheck-fringe-warning ((,class (:foreground ,yellow :background ,current-line))))

   ;; highlight indentation
   `(highlight-indentation-face ((,class (:background, current-line))))
   `(highlight-indentation-current-column-face ((,class (:background, far-background))))

   ;; Flymake
   `(flymake-warnline ((,class (:underline (:style wave :color ,orange) :background ,background))))
   `(flymake-errline ((,class (:underline (:style wave :color ,red) :background ,background))))

   ;; Clojure errors
   `(clojure-test-failure-face ((,class (:background nil :inherit flymake-warnline))))
   `(clojure-test-error-face ((,class (:background nil :inherit flymake-errline))))
   `(clojure-test-success-face ((,class (:background nil :foreground nil :underline ,green))))
   `(clojure-keyword-face ((,class (:inherit font-lock-builtin-face))))

   ;; EDTS errors
   `(edts-face-warning-line ((t (:background nil :inherit flymake-warnline))))
   `(edts-face-warning-mode-line ((,class (:background nil :foreground ,orange :weight bold))))
   `(edts-face-error-line ((t (:background nil :inherit flymake-errline))))
   `(edts-face-error-mode-line ((,class (:background nil :foreground ,red :weight bold))))

   ;; For Brian Carper's extended clojure syntax table
   `(clojure-keyword ((,class (:foreground ,yellow))))
   `(clojure-parens ((,class (:foreground ,foreground))))
   `(clojure-braces ((,class (:foreground ,green))))
   `(clojure-brackets ((,class (:foreground ,yellow))))
   `(clojure-double-quote ((,class (:foreground ,aqua :background nil))))
   `(clojure-special ((,class (:foreground ,blue))))
   `(clojure-java-call ((,class (:foreground ,purple))))
   ;; Rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,faded))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,"#1565C0"))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,"#EF6C00"))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,"#B388FF"))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,"#76ff03"))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,"#26A69A"))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,"#B71C1C"))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,"#795548"))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,"#827717"))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,foreground :background ,"#EF6C00"))))

   ;; MMM-mode
   `(mmm-code-submode-face ((,class (:background ,current-line))))
   `(mmm-comment-submode-face ((,class (:inherit font-lock-comment-face))))
   `(mmm-output-submode-face ((,class (:background ,current-line))))

   ;; Search
   `(match ((,class (:foreground ,background :background ,green :inverse-video nil))))
   `(isearch ((,class (:foreground ,background :background ,green))))
   `(isearch-lazy-highlight-face ((,class (:foreground ,background :background ,green :inverse-video nil))))
   `(lazy-highlight-face ((,class (:foreground ,background :background ,green :inverse-video nil))))
   `(isearch-fail ((,class (:background ,background :inherit font-lock-warning-face :inverse-video t))))

   ;; Evil
   `(evil-search-highlight-persist-highlight-face ((,class (:foreground ,background
                                                            :background ,green))))
   ;; iedit
   `(iedit-occurrence ((,class (:foreground ,background :background ,green))))

   ;; ahs
   `(ahs-face ((,class (:foreground ,background :background ,green))))
   `(ahs-plugin-whole-buffer-face ((,class (:foreground ,foreground :background ,green))))

   ;; Anzu
   `(anzu-mode-line ((,class (:foreground ,orange))))
   `(anzu-replace-highlight ((,class (:inherit isearch-lazy-highlight-face))))
   `(anzu-replace-to ((,class (:inherit isearch))))

   ;; IDO
   `(ido-subdir ((,class (:foreground ,purple))))
   `(ido-first-match ((,class (:foreground ,orange))))
   `(ido-only-match ((,class (:foreground ,green))))
   `(ido-indicator ((,class (:foreground ,red :background ,background))))
   `(ido-virtual ((,class (:foreground ,comment))))

   ;; flx-ido
   `(flx-highlight-face ((,class (:inherit nil :foreground ,yellow :weight bold :underline nil))))

   ;; which-function
   `(which-func ((,class (:foreground ,blue :background nil))))

   ;; Emacs interface
   `(cursor ((,class (:background ,dark))))
   `(fringe ((,class (:background ,background))))
   `(linum ((,class (:background ,current-line :foreground ,foreground))))
   `(linum-highlight-face ((,class (:background ,current-line :foreground ,foreground))))
   `(border ((,class (:background ,current-line))))
   `(vertical-border ((,class (:background ,selection
                                           :foreground ,selection))))
   `(border-glyph ((,class (nil))))
   `(highlight ((,class (:inverse-video nil :foreground ,foreground :background ,current-line))))
   `(hl-line ((,class (:inverse-video nil :background ,current-line))))
   `(gui-element ((,class (:background ,current-line :foreground ,foreground))))
   `(mode-line ((,class (:foreground ,foreground :background ,mode-line-background
                                     :box (:line-width 2 :color ,mode-line-background)))))
   `(mode-line-buffer-id ((,class (:foreground ,foreground :background nil :weight bold))))
   `(mode-line-inactive ((,class (:inherit mode-line
                                           :foreground ,subtle
                                           :background ,current-line
					   :box (:line-width 2 :color ,current-line)
                                           :weight normal))))
   `(mode-line-emphasis ((,class (:foreground ,foreground :slant italic))))
   `(mode-line-highlight ((,class (:foreground ,purple :box nil))))
   `(minibuffer-prompt ((,class (:foreground ,blue))))
   `(region ((,class (:background ,selection))))
   `(secondary-selection ((,class (:background ,secondary-selection))))

   `(header-line ((,class (:inherit mode-line :foreground ,purple :background nil))))

   `(trailing-whitespace ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-trailing ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-space-after-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-space-before-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-empty ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-line ((,class (:background nil :foreground ,red))))
   `(whitespace-indentation ((,class (:background nil :foreground ,aqua))))
   `(whitespace-space ((,class (:background nil :foreground ,selection))))
   `(whitespace-newline ((,class (:background nil :foreground ,selection))))
   `(whitespace-tab ((,class (:background nil :foreground ,selection))))
   `(whitespace-hspace ((,class (:background nil :foreground ,selection))))

   ;; Parenthesis matching (built-in)
   `(show-paren-match-face ((,class (:background ,blue :foreground ,background))))
   `(show-paren-mismatch-face ((,class (:background "red1" :foreground "white"))))

   ;; Smartparens paren matching
   `(sp-show-pair-match-face ((,class (:foreground ,background :background ,blue :inherit show-paren-match))))
   `(sp-show-pair-mismatch-face ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; Parenthesis matching (mic-paren)
   `(paren-face-match ((,class (:foreground nil :background nil :inherit show-paren-match))))
   `(paren-face-mismatch ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))
   `(paren-face-no-match ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; Parenthesis dimming (parenface)
   `(paren-face ((,class (:foreground ,comment :background nil))))

   `(sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
   `(sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
   `(slime-highlight-edits-face ((,class (:weight bold))))
   `(slime-repl-input-face ((,class (:weight normal :underline nil))))
   `(slime-repl-prompt-face ((,class (:underline nil :weight bold :foreground ,purple))))
   `(slime-repl-result-face ((,class (:foreground ,green))))
   `(slime-repl-output-face ((,class (:foreground ,blue :background ,background))))

   `(csv-separator-face ((,class (:foreground ,orange))))

   `(diff-hl-insert ((,class (:background ,green :foreground ,green))))
   `(diff-hl-change ((,class (:background ,blue :foreground ,blue))))
   `(diff-hl-delete ((,class (:background ,orange :foreground ,orange))))

   `(diff-added ((,class (:foreground ,green))))
   `(diff-changed ((,class (:foreground ,blue))))
   `(diff-removed ((,class (:foreground ,orange))))
   `(diff-header ((,class (:foreground ,aqua :background nil))))
   `(diff-hunk-header ((,class (:foreground ,purple))))
   `(diff-refine-added ((,class (:inherit diff-added :inverse-video t))))
   `(diff-refine-removed ((,class (:inherit diff-removed :inverse-video t))))

   `(ediff-even-diff-A ((,class (:foreground nil :background nil :inverse-video t))))
   `(ediff-even-diff-B ((,class (:foreground nil :background nil :inverse-video t))))
   `(ediff-odd-diff-A  ((,class (:foreground ,comment :background nil :inverse-video t))))
   `(ediff-odd-diff-B  ((,class (:foreground ,comment :background nil :inverse-video t))))

   `(eldoc-highlight-function-argument ((,class (:foreground ,green :weight bold))))

   ;; macrostep
   `(macrostep-expansion-highlight-face ((,class (:inherit highlight :foreground nil))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face ((,class (:foreground ,foreground))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,green :weight bold))))
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

   ;; dired+
   `(diredp-compressed-file-suffix ((,class (:foreground ,blue))))
   `(diredp-deletion ((,class (:inherit error :inverse-video t))))
   `(diredp-deletion-file-name ((,class (:inherit error))))
   `(diredp-dir-heading ((,class (:foreground ,green :weight bold))))
   `(diredp-dir-priv ((,class (:foreground ,aqua :background nil))))
   `(diredp-exec-priv ((,class (:foreground ,blue :background nil))))
   `(diredp-executable-tag ((,class (:foreground ,red :background nil))))
   `(diredp-file-name ((,class (:foreground ,yellow))))
   `(diredp-file-suffix ((,class (:foreground ,green))))
   `(diredp-flag-mark ((,class (:foreground ,green :inverse-video t))))
   `(diredp-flag-mark-line ((,class (:background nil :inherit highlight))))
   `(diredp-ignored-file-name ((,class (:foreground ,comment))))
   `(diredp-link-priv ((,class (:background nil :foreground ,purple))))
   `(diredp-mode-line-flagged ((,class (:foreground ,red))))
   `(diredp-mode-line-marked ((,class (:foreground ,green))))
   `(diredp-no-priv ((,class (:background nil))))
   `(diredp-number ((,class (:foreground ,yellow))))
   `(diredp-other-priv ((,class (:background nil :foreground ,purple))))
   `(diredp-rare-priv ((,class (:foreground ,red :background nil))))
   `(diredp-read-priv ((,class (:foreground ,green :background nil))))
   `(diredp-symlink ((,class (:foreground ,purple))))
   `(diredp-write-priv ((,class (:foreground ,yellow :background nil))))

   ;; diredfl
   `(diredfl-compressed-file-suffix ((,class (:foreground ,blue))))
   `(diredfl-compressed-file-name ((,class (:foreground ,blue))))
   `(diredfl-ignored-file-name ((,class (:foreground ,comment))))
   `(diredfl-date-time ((,class (:foreground ,green))))
   `(diredfl-file-name ((,class (:foreground ,foreground))))
   `(diredfl-read-priv ((,class (:foreground ,green :background nil))))
   `(diredfl-write-priv ((,class (:foreground ,yellow :background nil))))
   `(diredfl-exec-priv ((,class (:foreground ,red :background nil))))
   `(diredfl-rare-priv ((,class (:foreground ,orange :background nil))))
   `(diredfl-no-priv ((,class (:background nil))))
   `(diredfl-deletion ((,class (:inherit error :inverse-video t))))
   `(diredfl-deletion-file-name ((,class (:inherit error))))
   `(diredfl-dir-heading ((,class (:foreground ,green :weight bold))))
   `(diredfl-symlink ((,class (:foreground ,purple))))
   `(diredfl-dir-priv ((,class (:foreground ,aqua :background nil))))
   `(diredfl-dir-name ((,class (:foreground ,aqua :background nil))))
   `(diredfl-number ((,class (:foreground ,yellow :background nil))))
   `(diredfl-flag-mark ((,class (:foreground ,orange :background nil))))
   `(diredfl-flag-mark-line ((,class (:foreground ,nil :background ,selection))))
   `(diredfl-file-suffix ((,class (:foreground ,aqua :background nil))))

   ;; Magit
   `(magit-branch ((,class (:foreground ,green))))
   `(magit-diff-added ((,class (:inherit diff-added))))
   `(magit-diff-added-highlight ((,class (:inherit magit-diff-added
                                          :background ,far-background))))
   `(magit-diff-removed ((,class (:inherit diff-removed))))
   `(magit-diff-removed-highlight ((,class (:inherit magit-diff-removed
                                            :background ,far-background))))
   `(magit-header ((,class (:inherit nil :weight bold))))
   `(magit-item-highlight ((,class (:inherit highlight :background nil))))
   `(magit-log-author ((,class (:foreground ,aqua))))
   `(magit-log-graph ((,class (:foreground ,comment))))
   `(magit-log-date ((,class (:foreground ,yellow))))
   `(magit-section-title ((,class (:foreground ,blue :weight bold))))
   `(magit-section-highlight           ((t (:background ,current-line))))
   `(magit-section-heading             ((t (:foreground ,yellow :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,current-line  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,background
                                            :foreground ,orange :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,header-color))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,header-color))))
   `(magit-diff-hunk-heading-selection ((t (:background ,background
                                            :foreground ,orange))))
   `(magit-diff-lines-heading          ((t (:background ,orange
                                            :foreground ,background))))
   `(magit-blame-heading          ((t (:background ,far-background
                                                   :foreground ,aqua))))
   `(magit-blame-date             ((t (:background ,far-background
                                                   :foreground ,blue))))
   `(magit-blame-summary          ((t (:background ,far-background
                                                   :foreground ,green))))
   `(magit-diff-context-highlight      ((t (:background ,far-background
                                            :foreground "grey70"))))
   `(magit-diff-context                ((t (:foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,green))))
   `(magit-diffstat-removed ((t (:foreground ,red))))
   `(magit-process-ok    ((t (:foreground ,green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,red    :weight bold))))
   `(magit-branch-local  ((t (:foreground ,blue   :weight bold))))
   `(magit-branch-remote ((t (:foreground ,green  :weight bold))))
   `(magit-tag           ((t (:foreground ,orange :weight bold))))
   `(magit-hash          ((t (:foreground ,comment))))
   `(magit-sequence-stop ((t (:foreground ,green))))
   `(magit-sequence-part ((t (:foreground ,yellow))))
   `(magit-sequence-head ((t (:foreground ,blue))))
   `(magit-sequence-drop ((t (:foreground ,red))))

   ;; git-gutter
   `(git-gutter:modified ((,class (:foreground ,purple :weight bold))))
   `(git-gutter:added ((,class (:foreground ,green :background ,green :weight bold))))
   `(git-gutter:deleted ((,class (:foreground ,red :weight bold))))
   `(git-gutter:unchanged ((,class (:background ,yellow))))

   ;; git-gutter-fringe
   `(git-gutter-fr:modified ((,class (:foreground ,purple :weight bold))))
   `(git-gutter-fr:added ((,class (:foreground ,green :weight bold))))
   `(git-gutter-fr:deleted ((,class (:foreground ,red :weight bold))))

   `(link ((,class (:foreground nil :underline t))))
   `(widget-button ((,class (:underline t :weight bold))))
   `(widget-field ((,class (:background ,current-line :box (:line-width 1 :color ,foreground)))))

   ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
   `(compilation-column-number ((,class (:foreground ,yellow))))
   `(compilation-line-number ((,class (:foreground ,yellow))))
   `(compilation-message-face ((,class (:foreground ,blue))))
   `(compilation-mode-line-exit ((,class (:foreground ,green))))
   `(compilation-mode-line-fail ((,class (:foreground ,red))))
   `(compilation-mode-line-run ((,class (:foreground ,blue))))

   ;; Grep
   `(grep-context-face ((,class (:foreground ,comment))))
   `(grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,blue))))
   `(grep-match-face ((,class (:foreground nil :background nil :inherit match))))

   `(regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))

   ;; Helm
   `(helm-header ((,class (:foreground ,foreground :background ,background))))
   `(helm-selection ((,class (:background ,current-line))))
   `(helm-match ((,class (:foreground ,blue ))))
   `(helm-ff-file ((,class (:foreground ,foreground ))))
   `(helm-ff-directory ((,class (:foreground ,aqua ))))
   `(helm-ff-symlink ((,class (:foreground ,purple ))))
   `(helm-ff-executable ((,class (:foreground ,green ))))
   `(helm-buffer-directory ((,class (:foreground ,aqua))))
   `(helm-buffer-file ((,class (:foreground ,foreground))))
   `(helm-grep-file ((,class (:foreground ,aqua :underline t))))
   `(helm-buffer-process ((,class (:foreground ,red))))
   `(helm-buffer-not-saved ((,class (:foreground ,orange))))
   `(helm-candidate-number ((,class (:foreground ,background :background ,aqua))))
   `(helm-source-header ((,class (:background ,header-color :foreground ,"#424242" :height 1.3 :bold t ))))

   ;; guide-key
   `(guide-key/key-face ((,class (:foreground ,foreground ))))
   `(guide-key/highlight-command-face ((,class (:foreground ,orange ))))
   `(guide-key/prefix-command-face ((,class (:foreground ,blue ))))

   ;; which-key
   `(which-key-key-face ((,class (:foreground ,foreground  :weight bold))))
   `(which-key-special-key-face ((,class (:foreground ,orange  :weight bold :height 1.1))))
   `(which-key-command-description-face ((,class (:foreground ,foreground ))))
   `(which-key-group-description-face ((,class (:foreground ,blue ))))
   `(which-key-separator-face ((,class (:foreground ,comment ))))

   ;; mark-multiple
   `(mm/master-face ((,class (:inherit region :foreground nil :background nil))))
   `(mm/mirror-face ((,class (:inherit region :foreground nil :background nil))))

   `(org-agenda-structure ((,class (:foreground ,aqua :bold t))))
   `(org-agenda-date ((,class (:foreground ,blue :underline nil))))
   `(org-agenda-done ((,class (:foreground ,green))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
   `(org-block ((,class (:foreground ,foreground :background ,"#EFEBE9"))))
   `(org-block-background ((,t (:background ,"#EFEBE9"))))
   `(org-code ((,class (:foreground ,foreground :background ,"#EFEBE9"))))
   `(org-column ((,class (:background ,current-line))))
   `(org-column-title ((,class (:inherit org-column :weight bold :underline t))))
   `(org-date ((,class (:foreground ,"#80cbc4" :underline t))))
   `(org-document-info ((,class (:foreground ,aqua :height 1.35))))
   `(org-document-info-keyword ((,class (:foreground ,green :height 1.35))))
   `(org-document-title ((,class (:weight bold :foreground ,foreground :height 1.35))))
   `(org-done ((,class (:background ,"#c8e6c9" :bold t :foreground,"#2e7d32"))))
   `(org-ellipsis ((,class (:foreground ,comment))))
   `(org-footnote ((,class (:foreground ,aqua))))
   `(org-formula ((,class (:foreground ,red))))
   `(org-hide ((,class (:foreground ,background :background ,background))))
   `(org-link ((,class (:foreground ,red :underline t))))
   `(org-scheduled ((,class (:foreground ,green))))
   `(org-scheduled-previously ((,class (:foreground ,orange))))
   `(org-scheduled-today ((,class (:foreground ,green))))
   `(org-special-keyword ((,class (:foreground ,comment))))
   `(org-table ((,class (:foreground ,"#1565c0" :background ,"#e0f7fa"))))
   `(org-todo ((,class (:background ,"#ffcdd2" :bold t :foreground ,"#c62828"))))
   `(org-upcoming-deadline ((,class (:foreground ,orange))))
   `(org-warning ((,class (:weight bold :foreground ,red))))
   `(org-block-begin-line ((,class (:foreground ,"#4e342e" :background "#efebe9"
                                                :box (:style released-button)
                                                ))))
   `(org-block-end-line ((,class (:foreground ,"#4e342e" :background "#efebe9"
                                              :box (:style released-button)))))
   `(org-kbd ((,class (:background ,inactive-gray :foreground ,foreground
                                   :box (:line-width 1 :color nil :style pressed-button)))))

   `(org-level-1 ((,class (:inherit outline-1
                         :background ,inactive-gray
                         :weight bold
                         :box (:style released-button)
                         :height 1.3))))
   `(org-level-2 ((,class (:inherit outline-2
                                  :background ,"#C8E6C9"
                                  :box (:style released-button)
                         :height 1.2))))
   `(org-level-3 ((,class (:inherit outline-3  :height 1.1))))
   `(org-level-4 ((,class (:inherit outline-4  :height 1.0))))
   `(org-level-5 ((,class (:inherit outline-5 ))))
   `(org-level-6 ((,class (:inherit outline-6 ))))
   `(org-level-7 ((,class (:inherit outline-7 ))))
   `(org-level-8 ((,class (:inherit outline-8 ))))
   `(org-level-9 ((,class (:inherit outline-9 ))))

   `(markdown-header-face-1 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.3 ))))
   `(markdown-header-face-2 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.2 ))))
   `(markdown-header-face-3 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-4 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-5 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-6 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-7 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-8 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-9 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-delimiter-face ((,class (:inherit font-lock-function-name-face :weight bold
                                              :height 1.2))))
   `(markdown-url-face ((,class (:inherit link))))
   `(markdown-link-face ((,class (:foreground ,blue :underline t))))

   ;`(hl-sexp-face ((,class (:background ,current-line))))
   `(highlight-symbol-face ((,class (:background ,selection))))
   `(highlight-80+ ((,class (:background ,current-line))))

   ;; Python-specific overrides
   `(py-builtins-face ((,class (:foreground ,"#ff7043" :weight normal))))

   ;; ein (emacs-ipython-notebook) specific colors
   `(ein:cell-input-area ((,class (:background ,"#EFEFEF"))))
   `(ein:cell-input-prompt ((,class (:inherit org-block-begin-line))))
   `(ein:cell-output-prompt ((,class (:inherit org-block-end-line))))

   ;; js2-mode
   `(js2-warning ((,class (:underline ,orange))))
   `(js2-error ((,class (:foreground nil :underline ,red))))
   `(js2-external-variable ((,class (:foreground ,purple))))
   `(js2-function-param ((,class (:foreground ,blue))))
   `(js2-instance-member ((,class (:foreground ,blue))))
   `(js2-private-function-call ((,class (:foreground ,red))))

   ;; js3-mode
   `(js3-warning-face ((,class (:underline ,orange))))
   `(js3-error-face ((,class (:foreground nil :underline ,red))))
   `(js3-external-variable-face ((,class (:foreground ,purple))))
   `(js3-function-param-face ((,class (:foreground ,blue))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,orange))))
   `(js3-jsdoc-type-face ((,class (:foreground ,aqua))))
   `(js3-jsdoc-value-face ((,class (:foreground ,yellow))))
   `(js3-jsdoc-html-tag-name-face ((,class (:foreground ,blue))))
   `(js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,green))))
   `(js3-instance-member-face ((,class (:foreground ,blue))))
   `(js3-private-function-call-face ((,class (:foreground ,red))))

   ;; coffee-mode
   `(coffee-mode-class-name ((,class (:foreground ,orange :weight bold))))
   `(coffee-mode-function-param ((,class (:foreground ,purple))))

   ;; elixir-mode
   `(elixir-negation-face ((,class (:foreground ,red))))
   `(elixir-attribute-face ((,class (:foreground ,purple))))
   `(elixir-atom-face ((,class (:foreground ,aqua))))
   `(elixir-ignored-var-face ((,class (:foreground ,comment))))

   ;; nxml
   `(nxml-name-face ((,class (:foreground unspecified :inherit font-lock-constant-face))))
   `(nxml-attribute-local-name-face ((,class (:foreground unspecified :inherit font-lock-variable-name-face))))
   `(nxml-ref-face ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))))
   `(nxml-delimiter-face ((,class (:foreground unspecified :inherit font-lock-keyword-face))))
   `(nxml-delimited-data-face ((,class (:foreground unspecified :inherit font-lock-string-face))))
   `(rng-error-face ((,class (:underline ,red))))

   ;; RHTML
   `(erb-delim-face ((,class (:background ,current-line))))
   `(erb-exec-face ((,class (:background ,current-line :weight bold))))
   `(erb-exec-delim-face ((,class (:background ,current-line))))
   `(erb-out-face ((,class (:background ,current-line :weight bold))))
   `(erb-out-delim-face ((,class (:background ,current-line))))
   `(erb-comment-face ((,class (:background ,current-line :weight bold :slant italic))))
   `(erb-comment-delim-face ((,class (:background ,current-line))))

   ;; Message-mode
   `(message-header-other ((,class (:foreground nil :background nil :weight normal))))
   `(message-header-subject ((,class (:inherit message-header-other :weight bold :foreground ,yellow))))
   `(message-header-to ((,class (:inherit message-header-other :weight bold :foreground ,orange))))
   `(message-header-cc ((,class (:inherit message-header-to :foreground nil))))
   `(message-header-name ((,class (:foreground ,blue :background nil))))
   `(message-header-newsgroups ((,class (:foreground ,aqua :background nil :slant normal))))
   `(message-separator ((,class (:foreground ,purple))))

   ;; cfw emacs calendar
   `(cfw:face-title ((,class (:background ,background :foreground ,"#424242" :height 1.3 :weight bold))))
   `(cfw:face-today ((,class (:foreground ,foreground))))
   `(cfw:face-day-title ((,class (:background ,current-line :foreground ,foreground))))
   `(cfw:face-today-title ((,class (:background ,secondary-selection :foreground ,foreground))))
   `(cfw:face-header ((,class (:background ,current-line :foreground ,foreground))))
   `(cfw:face-sunday ((,class (:background ,current-line :foreground ,aqua :weight bold))))
   `(cfw:face-saturday ((,class (:background ,current-line :foreground ,aqua :weight bold))))
   `(cfw:face-select ((,class (:background ,selection :foreground ,foreground))))
   `(cfw:face-toolbar ((,class (:background ,aqua :foreground ,background :weight bold))))
   `(cfw:face-toolbar-button-off ((,class (:background ,aqua :foreground ,background :weight bold))))
   `(cfw:face-toolbar-button-on ((,class (:background ,aqua :foreground ,secondary-selection :weight bold))))
   `(cfw:face-holiday ((,class (:background ,current-line :foreground ,green :weight bold))))

   ;; Jabber
   `(jabber-chat-prompt-local ((,class (:foreground ,subtle))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,blue))))
   `(jabber-chat-prompt-system ((,class (:foreground ,orange :weight bold))))
   `(jabber-chat-text-local ((,class (:foreground ,subtle))))
   `(jabber-chat-text-foreign ((,class (:foreground ,foreground))))
   `(jabber-chat-text-error ((,class (:foreground ,red))))

   `(jabber-roster-user-online ((,class (:foreground ,green))))
   `(jabber-roster-user-xa ((,class :foreground ,comment)))
   `(jabber-roster-user-dnd ((,class :foreground ,yellow)))
   `(jabber-roster-user-away ((,class (:foreground ,orange))))
   `(jabber-roster-user-chatty ((,class (:foreground ,purple))))
   `(jabber-roster-user-error ((,class (:foreground ,red))))
   `(jabber-roster-user-offline ((,class (:foreground ,comment))))

   `(jabber-rare-time-face ((,class (:foreground ,comment))))
   `(jabber-activity-face ((,class (:foreground ,purple))))
   `(jabber-activity-personal-face ((,class (:foreground ,aqua))))

   ;; Company autocomplete
   ;; `(company-echo ((,class ())))
   ;; `(company-echo-common ((,class ())))
   `(company-preview ((,class (:foreground ,comment :background ,inactive-gray))))
   `(company-preview-common ((,class (:foreground ,comment :background ,inactive-gray)))) ; same background as highlight-line
   ;; `(company-preview-search ((,class ())))
   `(company-scrollbar-bg ((,class (:background "#F0F0F0"))))
   `(company-scrollbar-fg ((,class (:background "#C0C0C0"))))
   `(company-template-field ((,class (:background ,inactive-gray))))
   `(company-tooltip ((,class (:weight bold :foreground, comment :background ,inactive-gray))))
   `(company-tooltip-annotation ((,class (:weight normal :foreground ,comment :background ,inactive-gray))))
   `(company-tooltip-annotation-selection ((,class (:weight normal :inherit company-tooltip-selection))))
   `(company-tooltip-common ((,class (:weight normal :inherit company-tooltip))))
   `(company-tooltip-common-selection ((,class (:weight normal :inherit company-tooltip-selection))))
   ;; `(company-tooltip-mouse ((,class ())))
   ;; `(company-tooltip-search ((,class ())))
   `(company-tooltip-selection ((,class (:weight bold :foreground ,foreground :background ,far-background))))

   ;; Powerline
   `(powerline-active1 ((t (:foreground ,foreground :background ,inactive-gray))))
   `(powerline-active2 ((t (:foreground ,foreground :background ,inactive-gray))))
   `(powerline-inactive1 ((t (:foreground ,comment :background ,selection))))
   `(powerline-inactive2 ((t (:foreground ,comment :background ,selection))))

   ;; Spaceline
   `(spaceline-python-venv ((t (:foreground ,aqua))))
   `(spaceline-evil-normal ((t (:foreground ,background :background ,yellow :inherit mode-line))))
   `(spaceline-evil-insert ((t (:foreground ,background :background ,green :inherit mode-line))))
   `(spaceline-evil-visual ((t (:foreground ,background :background ,selection :inherit mode-line))))

   ;; Spacemacs
   `(spacemacs-normal-face ((t (:inherit spaceline-evil-normal))))
   `(spacemacs-insert-face ((t (:inherit spaceline-evil-insert))))
   `(spacemacs-visual-face ((t (:inherit spaceline-evil-visual))))

   ;; Outline
   `(outline-1 ((,class (:inherit nil :foreground ,foreground))))
   `(outline-2 ((,class (:inherit nil :foreground ,foreground))))
   `(outline-3 ((,class (:inherit nil :foreground ,foreground))))
   `(outline-4 ((,class (:inherit nil :foreground ,foreground))))
   `(outline-5 ((,class (:inherit nil :foreground ,foreground))))
   `(outline-6 ((,class (:inherit nil :foreground ,foreground))))
   `(outline-7 ((,class (:inherit nil :foreground ,foreground))))
   `(outline-8 ((,class (:inherit nil :foreground ,foreground))))
   `(outline-9 ((,class (:inherit nil :foreground ,foreground))))

   ;; Ledger-mode
   `(ledger-font-comment-face ((,class (:inherit font-lock-comment-face))))
   `(ledger-font-occur-narrowed-face ((,class (:inherit font-lock-comment-face :invisible t))))
   `(ledger-font-occur-xact-face ((,class (:inherit highlight))))
   `(ledger-font-payee-cleared-face ((,class (:foreground ,green))))
   `(ledger-font-payee-uncleared-face ((,class (:foreground ,aqua))))
   `(ledger-font-posting-account-cleared-face ((,class (:foreground ,blue))))
   `(ledger-font-posting-account-face ((,class (:foreground ,purple))))
   `(ledger-font-posting-account-pending-face ((,class (:foreground ,yellow))))
   `(ledger-font-xact-highlight-face ((,class (:inherit highlight))))
   `(ledger-occur-narrowed-face ((,class (:inherit font-lock-comment-face :invisible t))))
   `(ledger-occur-xact-face ((,class (:inherit highlight))))

   ;; auctex
   `(font-latex-bold-face                 ((t (:inherit bold))))
   `(font-latex-doctex-documentation-face ((t (:background unspecified))))
   `(font-latex-doctex-preprocessor-face ((t (:inherit (font-latex-doctex-documentation-face
                                                        font-lock-builtin-face font-lock-preprocessor-face)))))
   `(font-latex-italic-face               ((t (:inherit italic :underline nil :foreground ,foreground))))
   `(font-latex-math-face                 ((t (:foreground ,foreground :slant italic t))))
   `(font-latex-sectioning-0-face         ((t (:inherit outline-1 :height 1.4))))
   `(font-latex-sectioning-1-face         ((t (:inherit outline-2 :height 1.35))))
   `(font-latex-sectioning-2-face         ((t (:inherit outline-3 :height 1.3))))
   `(font-latex-sectioning-3-face         ((t (:inherit outline-4 :height 1.25))))
   `(font-latex-sectioning-4-face         ((t (:inherit outline-5 :height 1.2))))
   `(font-latex-sectioning-5-face         ((t (:inherit outline-6 :height 1.1))))
   `(font-latex-sedate-face               ((t (:foreground ,foreground :slant normal))))
   `(font-latex-slide-title-face          ((t (:inherit font-lock-type-face :weight bold :height 1.2))))
   `(font-latex-string-face               ((t (:inherit font-lock-string-face))))
   `(font-latex-script-char-face          ((t (:foreground ,faded))))
   `(font-latex-subscript-face            ((t (:height 0.8))))
   `(font-latex-superscript-face          ((t (:height 0.8))))
   `(font-latex-warning-face              ((t (:inherit font-lock-warning-face))))

   ;; emms
   `(emms-playlist-selected-face ((,class (:foreground ,orange))))
   `(emms-playlist-track-face ((,class (:foreground ,blue))))
   `(emms-browser-track-face ((,class (:foreground ,blue))))
   `(emms-browser-artist-face ((,class (:foreground ,red :height 1.3))))
   `(emms-browser-composer-face ((,class (:inherit emms-browser-artist-face))))
   `(emms-browser-performer-face ((,class (:inherit emms-browser-artist-face))))
   `(emms-browser-album-face ((,class (:foreground ,green :height 1.2))))

   ;; ;; stripe-buffer
   ;; `(stripe-highlight ((,class (:background ,current-line))))
   ;; `(stripe-hl-line ((,class (:background ,selection :foreground ,foreground))))

   ;; erc
   `(erc-direct-msg-face ((,class (:foreground ,orange))))
   `(erc-error-face ((,class (:foreground ,red))))
   `(erc-header-face ((,class (:foreground ,foreground :background ,selection))))
   `(erc-input-face ((,class (:foreground ,green))))
   `(erc-keyword-face ((,class (:foreground ,yellow))))
   `(erc-current-nick-face ((,class (:foreground ,green))))
   `(erc-my-nick-face ((,class (:foreground ,green))))
   `(erc-nick-default-face ((,class (:weight normal :foreground ,purple))))
   `(erc-nick-msg-face ((,class (:weight normal :foreground ,yellow))))
   `(erc-notice-face ((,class (:foreground ,comment))))
   `(erc-pal-face ((,class (:foreground ,orange))))
   `(erc-prompt-face ((,class (:foreground ,blue))))
   `(erc-timestamp-face ((,class (:foreground ,aqua))))
   `(erc-keyword-face ((,class (:foreground ,green))))

   ;; erc-colorize
   `(erc-distinct-1-face ((,class (:foreground ,"#E91E63"))))
   `(erc-distinct-2-face ((,class (:foreground ,"#1565C0"))))
   `(erc-distinct-3-face ((,class (:foreground ,"#827717"))))
   `(erc-distinct-4-face ((,class (:foreground ,"#B388FF"))))
   `(erc-distinct-5-face ((,class (:foreground ,"#EF6C00"))))
   `(erc-distinct-6-face ((,class (:foreground ,"#26A69A"))))
   `(erc-distinct-7-face ((,class (:foreground ,"#B71C1C"))))

   ;; twittering-mode
   `(twittering-username-face ((,class (:inherit erc-pal-face))))
   `(twittering-uri-face ((,class (:foreground ,blue :inherit link))))
   `(twittering-timeline-header-face ((,class (:foreground ,green :weight bold))))
   `(twittering-timeline-footer-face ((,class (:inherit twittering-timeline-header-face))))

   `(custom-variable-tag ((,class (:foreground ,blue))))
   `(custom-group-tag ((,class (:foreground ,blue))))
   `(custom-state ((,class (:foreground ,green))))

   ;; ansi-term
   `(term ((,class (:foreground nil :background nil :inherit default))))
   `(term-color-black   ((,class (:foreground ,foreground :background ,foreground))))
   `(term-color-red     ((,class (:foreground ,red :background ,red))))
   `(term-color-green   ((,class (:foreground ,green :background ,green))))
   `(term-color-yellow  ((,class (:foreground ,yellow :background ,yellow))))
   `(term-color-blue    ((,class (:foreground ,blue :background ,blue))))
   `(term-color-magenta ((,class (:foreground ,purple :background ,purple))))
   `(term-color-cyan    ((,class (:foreground ,aqua :background ,aqua))))
   `(term-color-white   ((,class (:foreground ,background :background ,background))))

   ;; elfeed
   `(elfeed-log-date-face ((,class (:foreground ,aqua))))
   `(elfeed-log-error-level-face ((,class (:foreground ,red))))
   `(elfeed-log-info-level-face ((,class (:foreground ,blue))))
   `(elfeed-log-warn-level-face ((,class (:foreground ,orange))))
   `(elfeed-search-date-face ((,class (:foreground ,purple))))
   `(elfeed-search-feed-face ((,class (:foreground ,yellow))))
   `(elfeed-search-tag-face ((,class (:foreground ,green))))

   ;; rpm-spec-mode
   `(rpm-spec-dir-face ((,class (:foreground ,green))))
   `(rpm-spec-doc-face ((,class (:foreground ,green))))
   `(rpm-spec-ghost-face ((,class (:foreground ,red))))
   `(rpm-spec-macro-face ((,class (:foreground ,yellow))))
   `(rpm-spec-obsolete-tag-face ((,class (:foreground ,red))))
   `(rpm-spec-package-face ((,class (:foreground ,red))))
   `(rpm-spec-section-face ((,class (:foreground ,yellow))))
   `(rpm-spec-tag-face ((,class (:foreground ,blue))))
   `(rpm-spec-var-face ((,class (:foreground ,red)))))

  (custom-theme-set-variables
   'white-paper
   `(fci-rule-color ,current-line)
   `(vc-annotate-color-map
     '((20  . ,red)
       (40  . ,orange)
       (60  . ,yellow)
       (80  . ,green)
       (100 . ,aqua)
       (120 . ,blue)
       (140 . ,purple)
       (160 . ,red)
       (180 . ,orange)
       (200 . ,yellow)
       (220 . ,green)
       (240 . ,aqua)
       (260 . ,blue)
       (280 . ,purple)
       (300 . ,red)
       (320 . ,orange)
       (340 . ,yellow)
       (360 . ,green)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; highlight-sexp-mode
   `(hl-sexp-background-color ,"#efebe9")

   `(ansi-color-names-vector (vector ,foreground ,red ,green ,yellow ,blue ,purple ,aqua ,background))
   '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'white-paper)

;; Local Variables:
;; rainbow-mode: t
;; hl-sexp-mode: nil
;; End:

;;; white-paper-theme.el ends here
