;;; early-init.el --- Emacs-Solo (no external packages) Configuration  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Early init configuration for Emacs-Solo
;;
;;; Code:

;; Startup hacks
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      vc-handled-backends '(Git))

(setq inhibit-compacting-font-caches t)

(set-face-attribute 'default nil :background "#19191c" :foreground "#f0ede5")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;;; early-init.el ends here
