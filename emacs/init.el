;; -*- lexical-binding: t; -*-

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Disable line numbers in some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Be resilient of missing packages.
(defmacro require-soft (name &rest body)
  `(if (require ,name nil t)
       (progn ,@body)
     (message "Could not load \"%s\", skipping..." ,name)))

;; (setq-default cursor-type 'bar)

(set-fontset-font t 'symbol "OpenMoji Color")

; (set-window-margins (selected-window) 3 3)

;; (require-soft 'evil)
;; (evil-mode 1)

;; (require-soft 'ivy)
;; (ivy-mode 1)

(require-soft 'rainbow-delimiters
	      (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "SF Mono" :foundry "APPL" :slant normal :weight normal :height 113 :width normal)))))

(put 'scroll-left 'disabled nil)

