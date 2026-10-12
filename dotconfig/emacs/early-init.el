;; -*- lexical-binding: t; -*-

;; Larger thresholds for garbage collection while Emacs is starting. This
;; reduces the number of GCs during startup and should improve startup time.
(setq
 gc-cons-threshold (* 1024 1024 128) ; 128mb
 gc-cons-percentage 1.0) ; Disable the dynamic percentage trigger to ensure GC frequency is fixed.

;; Tweak GC settings back during runtime.
(add-hook
 'after-init-hook
 (lambda ()
   (setq
    gc-cons-threshold (* 1024 1024 2) ; 2mb
    gc-cons-percentage 0.2)))

;; Single VC backend increases startup time and I only use git
(setq vc-handled-backends '(Git))

;; When in a GUI, disable tool bar; all these tools are in the menu-bar anyway
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; Always start Emacs and new frames maximized
(push '(fullscreen . maximized) default-frame-alist)
