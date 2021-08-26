;; -*- lexical-binding: t; -*-

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "PragmataPro Liga" :foundry "FSD " :slant normal :weight normal :height 128 :width normal))))
 '(show-paren-match ((((class color)) (:foreground "green"))))
 '(show-paren-mismatch ((((class color)) (:foreground "red")))))
(put 'scroll-left 'disabled nil)

(load-theme 'tango-dark)

;; (setq package-archive '(("melpa" . "https://melpa.org/packages/")
;; 			("org" . "https://orgmode.org/elpa/")
;; 			("elpa" . "https://elpa.gnu.org/packages/")))

;; (package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; ;; Initialize use-package on non-Linux platforms
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (setq use-package-always-ensure t)




(setq-default cursor-type 'bar)

;; below is the old stuff

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(fringe-mode 0 nil (fringe))
 '(global-display-line-numbers-mode t)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "SF Mono" :foundry "APPL" :slant normal :weight normal :height 121 :width normal)))))

(set-fontset-font t 'symbol "JoyPixels")
