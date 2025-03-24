;; -*- lexical-binding: t; -*-

;; Keybindings (from :bind)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-j") 'duplicate-dwim)
(global-set-key (kbd "M-g r") 'recentf)
(global-set-key (kbd "M-s g") 'grep)
(global-set-key (kbd "M-s f") 'find-name-dired)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x C-k RET"))

;; Custom variables (from :custom)
(setq ad-redefinition-action 'accept)
(setq column-number-mode t)
(setq line-number-mode t)
(setq completion-ignore-case t)
(setq completions-detailed t)
(setq completions-format 'one-column)
(setq delete-by-moving-to-trash t)
(setq display-line-numbers-width 3)
(setq display-line-numbers-widen t)
(setq delete-selection-mode 1)
(setq enable-recursive-minibuffers t)
(setq find-ls-option '("-exec ls -ldh {} +" . "-ldh"))
(setq frame-resize-pixelwise nil)
(setq global-auto-revert-non-file-buffers t)
(setq help-window-select t)
(setq history-length 300)
(setq inhibit-startup-message nil)
(setq initial-scratch-message "")
(setq kill-do-not-save-duplicates t)
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-use-momentum nil)
(setq ring-bell-function 'ignore)
(setq read-answer-short t)
(setq recentf-max-saved-items 300)
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup (if (daemonp) 300 'never))
(setq recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))
(setq remote-file-name-inhibit-delete-by-moving-to-trash t)
(setq remote-file-name-inhibit-auto-save t)
(setq resize-mini-windows 'grow-only)
(setq ring-bell-function #'ignore)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables
      '(kill-ring register-alist mark-ring global-mark-ring search-ring regexp-search-ring))
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)
(setq set-mark-command-repeat-pop t)
(setq split-width-threshold 170)
(setq split-height-threshold nil)
(setq shr-use-colors nil)
(setq switch-to-buffer-obey-display-actions t)
(setq tab-always-indent 'complete)
(setq tab-width 8)
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button-show nil)
(setq tab-bar-tab-hints t)
(setq treesit-font-lock-level 4)
(setq truncate-lines t)
(setq undo-limit 67108864)
(setq undo-strong-limit 100663296)
(setq undo-outer-limit 1006632960)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq use-short-answers t)
(setq visible-bell nil)
(setq window-combination-resize t)
(setq window-resize-pixelwise nil)
(setq xref-search-program 'ripgrep)
(setq grep-command "rg -nS --no-heading ")
(setq grep-find-ignored-directories
      '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))

(set-face-attribute 'default nil :family "PragmataPro Liga" :height 203)

(add-to-list 'save-some-buffers-action-alist
             (list "d"
                   (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                   "show diff between the buffer and its file"))

(set-display-table-slot standard-display-table 'vertical-border ?\u2502)
(set-display-table-slot standard-display-table 'truncation ?\u2192)

(setq ibuffer-saved-filter-groups
      '(("default"
         ("org" (or
                 (mode . org-mode)
                 (name . "^\\*Org Src")
                 (name . "^\\*Org Agenda\\*$")))
         ("tramp" (name . "^\\*tramp.*"))
         ("emacs" (or
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")
                   (name . "^\\*Warnings\\*$")
                   (name . "^\\*Shell Command Output\\*$")
                   (name . "^\\*Async-native-compile-log\\*$")
                   (name . "^\\*straight-")))
         ("ediff" (or
                   (name . "^\\*ediff.*")
                   (name . "^\\*Ediff.*")))
         ("dired" (mode . dired-mode))
         ("terminal" (or
                      (mode . term-mode)
                      (mode . shell-mode)
                      (mode . eshell-mode)))
         ("help" (or
                  (name . "^\\*Help\\*$")
                  (name . "^\\*info\\*$")
                  (name . "^\\*helpful"))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-show-empty-filter-groups nil)

(setenv "GIT_EDITOR" (format "emacs --init-dir=%s " (shell-quote-argument user-emacs-directory)))
(add-to-list 'auto-mode-alist '("/git-rebase-todo\\'" . conf-mode))

(add-hook 'after-init-hook
          (lambda ()
            (let ((private-file (expand-file-name "private.el" user-emacs-directory)))
              (when (file-exists-p private-file)
                (load private-file)))))

;; Initialization (from :init)
(set-window-margins (selected-window) 2 0)
(select-frame-set-input-focus (selected-frame))
(global-auto-revert-mode 1)
(setq indent-tabs-mode -1)
(recentf-mode 1)
(repeat-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(winner-mode)
(xterm-mouse-mode 1)
(file-name-shadow-mode 1)

;;; AUTH-SOURCE
(use-package auth-source
  :ensure nil
  :defer t
  :config
  (setq auth-sources
        (list (expand-file-name ".authinfo.gpg" user-emacs-directory)))
  (setq user-full-name "Xaver Hellauer"
        user-mail-address "xaver@hellauer.bayern")

  ;; Use `pass` as an auth-source
  (when (file-exists-p "~/.password-store")
    (auth-source-pass-enable)))


;;; CONF
(use-package conf-mode
  :ensure nil
  :mode ("\\.env\\..*\\'" "\\.env\\'")
  :init
  (add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode)))


;;; COMPILATION
(use-package compile
  :ensure nil
  :hook
  (;; Not ideal, but I do not want this poluting the modeline
   (compilation-start . (lambda () (setq compilation-in-progress nil))))
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (ansi-color-for-compilation-mode t)
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))


;;; WINDOW
(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(
     ;; ("\\*.*e?shell\\*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . 0.25)
     ;;  (side . bottom)
     ;;  (slot . -1))
     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\|Bookmark List\\|Occur\\|eldoc\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))
     ("\\*\\([Hh]elp\\)\\*"
      (display-buffer-in-side-window)
      (window-width . 75)
      (side . right)
      (slot . 0))
     ("\\*\\(Ibuffer\\)\\*"
      (display-buffer-in-side-window)
      (window-width . 100)
      (side . right)
      (slot . 1))
     ("\\*\\(Flymake diagnostics\\|xref\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
     ("\\*\\(grep\\|find\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 2))
     )))

;;; ICOMPLETE
(use-package icomplete
  :bind (:map icomplete-minibuffer-map
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)
              ("RET" . icomplete-force-complete-and-exit)
              ("C-j" . exit-minibuffer)) ;; So we can exit commands like `multi-file-replace-regexp-as-diff'
  :hook
  (after-init . (lambda ()
                  (fido-mode -1)
                  (icomplete-vertical-mode 1)))
  :config
  (setq icomplete-delay-completions-threshold 0)
  (setq icomplete-compute-delay 0)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-prospects-height 10)
  (setq icomplete-separator " . ")
  (setq icomplete-with-completion-tables t)
  (setq icomplete-in-buffer t)
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-scroll t)
  (advice-add 'completion-at-point
              :after #'minibuffer-hide-completions)

  ;; === FIXME: I'm reviewing it to the icomplete PATCH

(defface icomplete-vertical-selected-prefix-indicator-face
  '((t :inherit font-lock-keyword-face :weight bold :foreground "cyan"))
  "Face used for the prefix set by `icomplete-vertical-selected-prefix-indicator'."
  :group 'icomplete
  :version "31.1")

(defface icomplete-vertical-unselected-prefix-indicator-face
  '((t :inherit font-lock-keyword-face :weight normal :foreground "gray"))

  "Face used for the prefix set by `icomplete-vertical-unselected-prefix-indicator'."
  :group 'icomplete
  :version "31.1")

(defcustom icomplete-vertical-in-buffer-adjust-list t
  "Control whether in-buffer completion should align the cursor position.
If this is t and `icomplete-in-buffer' is t, and `icomplete-vertical-mode'
is activated, the in-buffer vertical completions are shown aligned to the
cursor position when the completion started, not on the first column, as
the default behaviour."
  :type 'boolean
  :group 'icomplete
  :version "31.1")

(defcustom icomplete-vertical-render-prefix-indicator t
  "Control whether a indicator is added as a prefix to each candidate.
If this is t and `icomplete-vertical-mode' is activated, a indicator,
controlled by `icomplete-vertical-selected-prefix-indicator' is shown
as a prefix to the current under selection candidate, while the
remaining of the candidates will receive the indicator controlled
by `icomplete-vertical-unselected-prefix-indicator'."
  :type 'boolean
  :group 'icomplete
  :version "31.1")

(defcustom icomplete-vertical-selected-prefix-indicator "» "
  "Prefix string used to mark the selected completion candidate.
If `icomplete-vertical-render-prefix-indicator' is t, the string
defined here is used as a prefix of the currently selected entry in the
list.  It can be further customized by the face
`icomplete-vertical-selected-prefix-indicator-face'."
  :type 'string
  :group 'icomplete
  :version "31.1")

(defcustom icomplete-vertical-unselected-prefix-indicator "  "
  "Prefix string used on the unselected completion candidates.
If `icomplete-vertical-render-prefix-indicator' is t, the string
defined here is used as a prefix for all unselected entries in the list.
list.  It can be further customized by the face
`icomplete-vertical-unselected-prefix-indicator-face'."
  :type 'string
  :group 'icomplete
  :version "31.1")

;; FIXME: make this into PATCH - OK
(defun icomplete-vertical--adjust-lines-for-column (lines buffer data)
  "Adjust the LINES to align with the column in BUFFER based on DATA."
  (if icomplete-vertical-in-buffer-adjust-list
      (let* ((column (current-column))
             (prefix-indicator-width
              (if icomplete-vertical-render-prefix-indicator
                  (max (length icomplete-vertical-selected-prefix-indicator)
                       (length icomplete-vertical-unselected-prefix-indicator))
                0))
             (wrapped-line (with-current-buffer buffer
                             (save-excursion
                               (goto-char (car data))

                               (beginning-of-line)
                               (count-screen-lines (point) (car data)))))
             (window-width (+ (window-hscroll) (window-body-width)))
             (longest-line-width (apply #'max (mapcar #'length lines)))
             (spaces-to-add
              (if (> wrapped-line 1)
                  (- column (* (- wrapped-line 1) (- window-width 5)))
                column))
             (spaces-to-add-avoiding-scrolling
              (if (>= (+ spaces-to-add longest-line-width prefix-indicator-width) window-width)
                  (- spaces-to-add longest-line-width)
                spaces-to-add)))

        (mapcar (lambda (line)
                  (concat (make-string spaces-to-add-avoiding-scrolling ?\s) line))
                lines))
    lines))

;; FIXME: what to demo/test:
;;
;; This patch provides two more new features, which improves icomplete-vertical-mode, 1 and 2,
;; explained below:
;;
;;
;; 1.) Improve feature provided by `icomplete-in-buffer'.
;;     If user, besides setting `icomplete-in-buffer' to t, also set the
;;     new `icomplete-vertical-in-buffer-adjust-list' to t, the following are fixed/ improved:
;;
;; Without the new `icomplete-vertical-in-buffer-adjust-list':
;; - [ ] wrapped lines   - completion candidates on different columns always shows candidates at column 0
;; - [ ] wrapped lines   - completion candidates on different lines always shows candidates at column 0
;; - [ ] wrapped lines   - completion candidates close to the end of buffer won't be printed
;; - [ ] truncated lines - completion candidates on different columns always shows candidates at column 0
;; - [ ] truncated lines - completion candidates on horizontally scrolled windows won't appear on buffer
;;                         as they're on column 0
;; - [ ] truncated lines - completion candidates close to the end of buffer wont be shown
;;
;;
;; With the new `icomplete-vertical-in-buffer-adjust-list':
;; - [ ] wrapped lines   - fix    : completion candidates on different columns will always be printed
;;                                  under the cursor
;; - [ ] wrapped lines   - feature: completion candidates on different columns close to the end
;;                                  of the buffer will adjust so they stay visible
;; - [ ] wrapped lines   - fix:   : completion candidates on different lines always be printed under
;;                                  the cursor
;; - [ ] wrapped lines   - fix    : if icomplete-prospects-height won't fit from current line to the
;;                                  end of vertical space, our window will be scrolled so we have at
;;                                  least this amount of lines. This ensures our candidates list is
;;                                  always visible
;; - [ ] truncated lines - fix    : completion candidates on different columns will always be printed
;;                                  under the cursor
;; - [ ] truncated lines - feature: completion candidates on different columns close to the end
;;                                  of the buffer will adjust so they stay visible even when we scroll
;;                                  horizontally
;; - [ ] truncated lines - feature: completion candidates on horizontally scrolled windows will be
;;                                  printed under the cursor
;; - [ ] wrapped lines   - feature: if icomplete-prospects-height won't fit from current line to the
;;                                  end of vertical space, our window will be scrolled so we have at
;;                                  least this amount of lines. This ensures our candidates list is
;;                                  always visible
;; - [ ] from wrapped    - feature: if we are on wrapped lines and manually horiontal scroll, the lines
;;       to truncated               will become automatically truncated, in this case, all the features
;;                                  above still works from either mode (wrapped or truncated).
;;
;;
;; 2.) Implements new feature which provides customizable prefix indicators
;;
;; Setting `icomplete-vertical-render-prefix-indicator' to t will provide a prefix indicator
;; to indicate the current selected candidate, by default "» ".
;;
;; This prefix is customizable through the variable `icomplete-vertical-selected-prefix-indicator'
;; and de face `icomplete-vertical-selected-prefix-indicator-face'.
;;
;; Users can also customize an indicator to the not selected candidates trhough the use of
;; the variable `icomplete-vertical-unselected-prefix-indicator', by default: "  ", and the face
;; `icomplete-vertical-unselected-prefix-indicator-face'.
;;


;; FIXME: remove this after patch
(defun icomplete-vertical--ensure-visible-lines-inside-buffer ()
  "Ensure the completion list is visible in regular buffers only.
Scrolls the screen to be at least `icomplete-prospects-height' real lines
away from the bottom.  Counts wrapped lines as real lines."
  (unless (minibufferp)
    (let* ((window-height (window-body-height))
           (current-line (count-screen-lines (window-start) (point)))
           (lines-to-bottom (- window-height current-line)))
      (when (< lines-to-bottom icomplete-prospects-height)
        (scroll-up (- icomplete-prospects-height lines-to-bottom))))))


(defun icomplete-vertical--add-indicator-to-selected (comp)
  "Add indicators to the selected/unselected COMP completions."
  (if (and icomplete-vertical-render-prefix-indicator
           (get-text-property 0 'icomplete-selected comp))
      (concat (propertize icomplete-vertical-selected-prefix-indicator
                          'face 'icomplete-vertical-selected-prefix-indicator-face)
              comp)
    (concat (propertize icomplete-vertical-unselected-prefix-indicator
                        'face 'icomplete-vertical-unselected-prefix-indicator-face)
            comp)))


(cl-defun icomplete--render-vertical
    (comps md &aux scroll-above scroll-below
           (total-space ; number of mini-window lines available
            (1- (min
                 icomplete-prospects-height
                 (truncate (max-mini-window-lines) 1)))))
  ;; Welcome to loopapalooza!
  ;;
  ;; First, be mindful of `icomplete-scroll' and manual scrolls.  If
  ;; `icomplete--scrolled-completions' and `icomplete--scrolled-past'
  ;; are:
  ;;
  ;; - both nil, there is no manual scroll;
  ;; - both non-nil, there is a healthy manual scroll that doesn't need
  ;;   to be readjusted (user just moved around the minibuffer, for
  ;;   example);
  ;; - non-nil and nil, respectively, a refiltering took place and we
  ;;   may need to readjust them to the new filtered `comps'.
  (when (and icomplete-scroll                                    ;; FIXME: remove this after patch
             (not icomplete--scrolled-completions)
             (not icomplete--scrolled-past))
    (icomplete-vertical--ensure-visible-lines-inside-buffer))
  (when (and icomplete-scroll
             icomplete--scrolled-completions
             (null icomplete--scrolled-past))
    (icomplete-vertical--ensure-visible-lines-inside-buffer)     ;; FIXME: remove this after patch
    (cl-loop with preds
             for (comp . rest) on comps
             when (equal comp (car icomplete--scrolled-completions))
             do
             (setq icomplete--scrolled-past preds
                   comps (cons comp rest))
             (completion--cache-all-sorted-completions
              (icomplete--field-beg)
              (icomplete--field-end)
              comps)
             and return nil
             do (push comp preds)
             finally (setq icomplete--scrolled-completions nil)))
  ;; Then, in this pretty ugly loop, collect completions to display
  ;; above and below the selected one, considering scrolling
  ;; positions.
  (cl-loop with preds = icomplete--scrolled-past
           with succs = (cdr comps)
           with space-above = (- total-space
                                 1
                                 (cl-loop for (_ . r) on comps
                                          repeat (truncate total-space 2)
                                          while (listp r)
                                          count 1))
           repeat total-space
           for neighbor = nil
           if (and preds (> space-above 0)) do
           (push (setq neighbor (pop preds)) scroll-above)
           (cl-decf space-above)
           else if (consp succs) collect
           (setq neighbor (pop succs)) into scroll-below-aux
           while neighbor
           finally (setq scroll-below scroll-below-aux))
  ;; Halfway there...
  (let* ((selected (propertize (car comps) 'icomplete-selected t))
         (chosen (append scroll-above (list selected) scroll-below))
         (tuples (icomplete--augment md chosen))
         max-prefix-len max-comp-len lines nsections)
    (add-face-text-property 0 (length selected)
                            'icomplete-selected-match 'append selected)
    ;; Figure out parameters for horizontal spacing
    (cl-loop
     for (comp prefix) in tuples
     maximizing (length prefix) into max-prefix-len-aux
     maximizing (length comp) into max-comp-len-aux
     finally (setq max-prefix-len max-prefix-len-aux
                   max-comp-len max-comp-len-aux))
    ;; Serialize completions and section titles into a list
    ;; of lines to render
    (cl-loop
     for (comp prefix suffix section) in tuples
     when section
     collect (propertize section 'face 'icomplete-section) into lines-aux
     and count 1 into nsections-aux
     for comp = (icomplete-vertical--add-indicator-to-selected comp)
     when (get-text-property 0 'icomplete-selected comp)
     do (add-face-text-property 0 (length comp)
                                'icomplete-selected-match 'append comp)
     collect (concat prefix
                     (make-string (max 0 (- max-prefix-len (length prefix))) ? )
                     (completion-lazy-hilit comp)
                     (make-string (max 0 (- max-comp-len (length comp))) ? )
                     suffix)
     into lines-aux
     finally (setq lines lines-aux
                   nsections nsections-aux))
    ;; Kick out some lines from the beginning due to extra sections.
    ;; This hopes to keep the selected entry more or less in the
    ;; middle of the dropdown-like widget when `icomplete-scroll' is
    ;; t.  Funky, but at least I didn't use `cl-loop'
    (setq lines
          (nthcdr
           (cond ((<= (length lines) total-space) 0)
                 ((> (length scroll-above) (length scroll-below)) nsections)
                 (t (min (ceiling nsections 2) (length scroll-above))))
           lines))
    (when icomplete--in-region-buffer
      (setq lines (icomplete-vertical--adjust-lines-for-column
                   lines icomplete--in-region-buffer completion-in-region--data)))
    ;; At long last, render final string return value.  This may still
    ;; kick out lines at the end.
    (concat " \n"
            (cl-loop for l in lines repeat total-space concat l concat "\n"))))

;; end use-package
)

;;; DIRED
(use-package dired
  :ensure nil
  :bind
  (("M-i" . emacs-solo/window-dired-vc-root-left))
  :custom
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open")
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open")
     (".*" "xdg-open" "open")))
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-al --group-directories-first")
  :init
  (defun emacs-solo/window-dired-vc-root-left (&optional directory-path)
    "Creates *Dired-Side* like an IDE side explorer"
    (interactive)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)

    (let ((dir (if directory-path
                   (dired-noselect directory-path)
         (if (eq (vc-root-dir) nil)
                     (dired-noselect default-directory)
                   (dired-noselect (vc-root-dir))))))

      (display-buffer-in-side-window
       dir `((side . left)
         (slot . 0)
         (window-width . 30)
         (window-parameters . ((no-other-window . t)
                   (no-delete-other-windows . t)
                   (mode-line-format . (" "
                            "%b"))))))
      (with-current-buffer dir
    (let ((window (get-buffer-window dir)))
          (when window
            (select-window window)
        (rename-buffer "*Dired-Side*")
        )))))

  (defun emacs-solo/window-dired-open-directory ()
    "Open the current directory in *Dired-Side* side window."
    (interactive)
    (emacs-solo/window-dired-vc-root-left (dired-get-file-for-visit)))

  (eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "G") 'emacs-solo/window-dired-open-directory))))


;;; WDIRED
(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))


;;; ESHELL
(use-package eshell
  :ensure nil
  :defer t
  :config
  (defun emacs-solo/eshell-pick-history ()
    "Show Eshell history in a completing-read picker and insert the selected command."
    (interactive)
    (let* ((history-file (expand-file-name "eshell/history" user-emacs-directory))
           (history-entries (when (file-exists-p history-file)
                              (with-temp-buffer
                                (insert-file-contents history-file)
                                (split-string (buffer-string) "\n" t))))
           (selection (completing-read "Eshell History: " history-entries)))
      (when selection
        (insert selection))))


  (defun eshell/cat-with-syntax-highlighting (filename)
    "Like cat(1) but with syntax highlighting.
  Stole from aweshell"
    (let ((existing-buffer (get-file-buffer filename))
          (buffer (find-file-noselect filename)))
      (eshell-print
       (with-current-buffer buffer
         (if (fboundp 'font-lock-ensure)
             (font-lock-ensure)
           (with-no-warnings
             (font-lock-fontify-buffer)))
         (let ((contents (buffer-string)))
           (remove-text-properties 0 (length contents) '(read-only nil) contents)
           contents)))
      (unless existing-buffer
        (kill-buffer buffer))
      nil))
  (advice-add 'eshell/cat :override #'eshell/cat-with-syntax-highlighting)


  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c l") #'emacs-solo/eshell-pick-history)
              (local-set-key (kbd "C-l")
                             (lambda ()
                               (interactive)
                               (eshell/clear 1)
                               (eshell-send-input)))))

  (require 'vc)
  (require 'vc-git)
  (setopt eshell-prompt-function
        (lambda ()
          (concat
           "┌─("
           (if (> eshell-last-command-status 0)
               "❌"
             "🐂")
           " " (number-to-string eshell-last-command-status)
           ")──("
           "🧘 " (or (file-remote-p default-directory 'user) (user-login-name))
           ")──("
           "💻 " (or (file-remote-p default-directory 'host) (system-name))
           ")──("
           "🕝 " (format-time-string "%H:%M:%S" (current-time))
           ")──("
           "📁 "
           (concat (if (>= (length (eshell/pwd)) 40)
                       (concat "..." (car (last (butlast (split-string (eshell/pwd) "/") 0))))
                     (abbreviate-file-name (eshell/pwd))))
           ")\n"

           (when (and (fboundp 'vc-git-root) (vc-git-root default-directory))
             (concat
              "├─(🌿 " (car (vc-git-branches))
              (let* ((branch (car (vc-git-branches)))
                     (behind (string-to-number
                              (shell-command-to-string
                               (concat "git rev-list --count HEAD..origin/" branch)))))
                (if (> behind 0)
                    (concat "  ⬇️ " (number-to-string behind))))

              (let ((modified (length (split-string
                                       (shell-command-to-string
                                        "git ls-files --modified") "\n" t)))
                    (untracked (length (split-string
                                        (shell-command-to-string
                                         "git ls-files --others --exclude-standard") "\n" t))))
                (concat
                 (if (> modified 0)
                     (concat "  ✏️ " (number-to-string modified)))
                 (if (> untracked 0)
                     (concat "  📄 " ))))
              ")\n"))
           "└─➜ ")))

  (setq eshell-prompt-regexp "└─➜ ")

  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))

  (setq eshell-visual-commands
        '("vi" "screen" "top"  "htop" "btm" "less" "more" "lynx" "ncftp" "pine" "tin" "trn"
          "elm" "irssi" "nmtui-connect" "nethack" "vim" "alsamixer" "nvim" "w3m"
          "ncmpcpp" "newsbeuter" "nethack" "mutt")))

;;; ISEARCH
(use-package isearch
  :ensure nil
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq search-whitespace-regexp ".*?")

  (defun isearch-copy-selected-word ()
    "Copy the current `isearch` selection to the kill ring."
    (interactive)
    (when isearch-other-end
      (let ((selection (buffer-substring-no-properties isearch-other-end (point))))
        (kill-new selection)
        (isearch-exit))))

  ;; Bind `M-w` in isearch to copy the selected word, so M-s M-. M-w
  ;; does a great job of 'copying the current word under cursor'.
  (define-key isearch-mode-map (kbd "M-w") 'isearch-copy-selected-word))


;;; VC
(use-package vc
  :ensure nil
  :defer t
  :config
  (setopt
   vc-git-diff-switches '("--patch-with-stat" "--histogram")  ;; add stats to `git diff'
   vc-git-log-switches '("--stat")                            ;; add stats to `git log'
   vc-git-log-edit-summary-target-len 50
   vc-git-log-edit-summary-max-len 70
   vc-git-print-log-follow t
   vc-git-revision-complete-only-branches nil
   vc-annotate-display-mode 'scale
   add-log-keep-changes-together t
   vc-make-backup-files nil)                                  ;; Do not backup version controlled files

  (with-eval-after-load 'vc-annotate
    (setopt vc-annotate-color-map
          '((20 . "#c3e88d")
            (40 . "#89DDFF")
            (60 . "#82aaff")
            (80 . "#676E95")
            (100 . "#c792ea")
            (120 . "#f78c6c")
            (140 . "#79a8ff")
            (160 . "#f5e0dc")
            (180 . "#a6e3a1")
            (200 . "#94e2d5")
            (220 . "#89dceb")
            (240 . "#74c7ec")
            (260 . "#82aaff")
            (280 . "#b4befe")
            (300 . "#b5b0ff")
            (320 . "#8c9eff")
            (340 . "#6a81ff")
            (360 . "#5c6bd7"))))

  ;; This one is for editing commit messages
  (require 'log-edit)
  (setopt log-edit-confirm 'changed
          log-edit-keep-buffer nil
          log-edit-require-final-newline t
          log-edit-setup-add-author nil)

  ;; Removes the bottom window with modified files list
  (remove-hook 'log-edit-hook #'log-edit-show-files)

  (with-eval-after-load 'vc-dir
    ;; In vc-git and vc-dir for git buffers, make (C-x v) a run git add, u run git
    ;; reset, and r run git reset and checkout from head.
    (defun emacs-solo/vc-git-command (verb fn)
      "Execute a Git command with VERB as action description and FN as operation on files."
      (let* ((fileset (vc-deduce-fileset t)) ;; Deduce fileset
             (backend (car fileset))
             (files (nth 1 fileset)))
        (if (eq backend 'Git)
            (progn
              (funcall fn files)
              (message "%s %d file(s)." verb (length files)))
          (message "Not in a VC Git buffer."))))

    (defun emacs-solo/vc-git-add (&optional revision vc-fileset comment)
      (interactive "P")
      (emacs-solo/vc-git-command "Staged" 'vc-git-register))

    (defun emacs-solo/vc-git-reset (&optional revision vc-fileset comment)
      (interactive "P")
      (emacs-solo/vc-git-command "Unstaged"
                                 (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))


    ;; Bind S and U in vc-dir-mode-map
    (define-key vc-dir-mode-map (kbd "S") #'emacs-solo/vc-git-add)
    (define-key vc-dir-mode-map (kbd "U") #'emacs-solo/vc-git-reset)

    ;; Bind S and U in vc-prefix-map for general VC usage
    (define-key vc-prefix-map (kbd "S") #'emacs-solo/vc-git-add)
    (define-key vc-prefix-map (kbd "U") #'emacs-solo/vc-git-reset)

    ;; Bind g to hide up to date files after refreshing in vc-dir
    (define-key vc-dir-mode-map (kbd "g")
                (lambda () (interactive) (vc-dir-refresh) (vc-dir-hide-up-to-date)))


    (defun emacs-solo/vc-git-visualize-status ()
      "Show the Git status of files in the `vc-log` buffer."
      (interactive)
      (let* ((fileset (vc-deduce-fileset t))
             (backend (car fileset))
             (files (nth 1 fileset)))
        (if (eq backend 'Git)
            (let ((output-buffer "*Git Status*"))
              (with-current-buffer (get-buffer-create output-buffer)
                (read-only-mode -1)
                (erase-buffer)
                ;; Capture the raw output including colors using 'git status --color=auto'
                (call-process "git" nil output-buffer nil "status" "-v")
                (pop-to-buffer output-buffer)))
          (message "Not in a VC Git buffer."))))

    (define-key vc-dir-mode-map (kbd "V") #'emacs-solo/vc-git-visualize-status)
    (define-key vc-prefix-map (kbd "V") #'emacs-solo/vc-git-visualize-status))

  (defun emacs-solo/vc-git-reflog ()
    "Show git reflog in a new buffer with ANSI colors and custom keybindings."
    (interactive)
    (let* ((root (vc-root-dir)) ;; Capture VC root before creating buffer
           (buffer (get-buffer-create "*vc-git-reflog*")))
      (with-current-buffer buffer
        (setq-local vc-git-reflog-root root) ;; Store VC root as a buffer-local variable
        (let ((inhibit-read-only t))
          (erase-buffer)
          (vc-git-command buffer nil nil
                          "reflog"
                          "--color=always"
                          "--pretty=format:%C(yellow)%h%Creset %C(auto)%d%Creset %Cgreen%gd%Creset %s %Cblue(%cr)%Creset")
          (goto-char (point-min))
          (ansi-color-apply-on-region (point-min) (point-max)))

        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "/") #'isearch-forward)
          (define-key map (kbd "p") #'previous-line)
          (define-key map (kbd "n") #'next-line)
          (define-key map (kbd "q") #'kill-buffer-and-window)

          (use-local-map map))

        (setq buffer-read-only t)
        (setq mode-name "Git-Reflog")
        (setq major-mode 'special-mode))
      (pop-to-buffer buffer)))
  (global-set-key (kbd "C-x v R") 'emacs-solo/vc-git-reflog)


  (defun emacs-solo/vc-pull-merge-current-branch ()
  "Pull the latest change from origin for the current branch and display output in a buffer."
  (interactive)
  (let* ((branch (vc-git--symbolic-ref "HEAD"))
         (buffer (get-buffer-create "*Git Pull Output*"))
         (command (format "git pull origin %s" branch)))
    (if branch
        (progn
          (with-current-buffer buffer
            (erase-buffer)
            (insert (format "$ %s\n\n" command))
            (call-process-shell-command command nil buffer t))
          (display-buffer buffer))
      (message "Could not determine current branch."))))


  (defun emacs-solo/vc-browse-remote (&optional current-line)
  "Open the repository's remote URL in the browser.
If CURRENT-LINE is non-nil, point to the current branch, file, and line.
Otherwise, open the repository's main page."
  (interactive "P")
  (let* ((remote-url (string-trim (vc-git--run-command-string nil "config" "--get" "remote.origin.url")))
         (branch (string-trim (vc-git--run-command-string nil "rev-parse" "--abbrev-ref" "HEAD")))
         (file (string-trim (file-relative-name (buffer-file-name) (vc-root-dir))))
         (line (line-number-at-pos)))
    (message "Opening remote on browser: %s" remote-url)
    (if (and remote-url (string-match "\\(?:git@\\|https://\\)\\([^:/]+\\)[:/]\\(.+?\\)\\(?:\\.git\\)?$" remote-url))
        (let ((host (match-string 1 remote-url))
              (path (match-string 2 remote-url)))
          ;; Convert SSH URLs to HTTPS (e.g., git@github.com:user/repo.git -> https://github.com/user/repo)
          (when (string-prefix-p "git@" host)
            (setq host (replace-regexp-in-string "^git@" "" host)))
          ;; Construct the appropriate URL based on CURRENT-LINE
          (browse-url
           (if current-line
               (format "https://%s/%s/blob/%s/%s#L%d" host path branch file line)
             (format "https://%s/%s" host path))))
      (message "Could not determine repository URL"))))
  (global-set-key (kbd "C-x v B") 'emacs-solo/vc-browse-remote)
  (global-set-key (kbd "C-x v o")
                  '(lambda () (interactive) (emacs-solo/vc-browse-remote 1)))


  (defun emacs-solo/vc-diff-on-current-hunk ()
    "Show the diff for the current file and jump to the hunk containing the current line."
    (interactive)
    (let ((current-line (line-number-at-pos)))
      (message "Current line in file: %d" current-line)
      (vc-diff) ; Generate the diff buffer
      (with-current-buffer "*vc-diff*"
        (goto-char (point-min))
        (let ((found-hunk nil))
          (while (and (not found-hunk)
                      (re-search-forward "^@@ -\\([0-9]+\\), *[0-9]+ \\+\\([0-9]+\\), *\\([0-9]+\\) @@" nil t))
            (let* ((start-line (string-to-number (match-string 2)))
                   (line-count (string-to-number (match-string 3)))
                   (end-line (+ start-line line-count)))
              (message "Found hunk: %d to %d" start-line end-line)
              (when (and (>= current-line start-line)
                         (<= current-line end-line))
                (message "Current line %d is within hunk range %d to %d" current-line start-line end-line)
                (setq found-hunk t)
                (goto-char (match-beginning 0))))) ; Jump to the beginning of the hunk
          (unless found-hunk
            (message "Current line %d is not within any hunk range." current-line)
            (goto-char (point-min)))))))
  (global-set-key (kbd "C-x v =") 'emacs-solo/vc-diff-on-current-hunk))

;;; SMERGE
(use-package smerge-mode
  :ensure nil
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)
              ("C-c ^ l" . smerge-keep-lower)
              ("C-c ^ n" . smerge-next)
              ("C-c ^ p" . smerge-previous)))

;;; DIFF
(use-package diff-mode
  :ensure nil
  :defer t
  :config
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  (setq diff-font-lock-syntax 'hunk-also)
  (setq diff-font-lock-prettify nil))

;;; EDIFF
(use-package ediff
  :ensure nil
  :commands (ediff-buffers ediff-files ediff-buffers3 ediff-files3)
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t))

;;; ELDOC
(use-package eldoc
  :ensure nil
  :init
  (global-eldoc-mode))

;;; EGLOT
(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-prefer-plaintext t)
  (jsonrpc-event-hook nil)
  (eglot-code-action-indications nil) ;; Emacs 31 -- annoying as hell
  :init
  (fset #'jsonrpc--log-event #'ignore)

  (defun emacs-solo/eglot-setup ()
    "Setup eglot mode with specific exclusions."
    (unless (eq major-mode 'emacs-lisp-mode)
      (eglot-ensure)))

  (add-hook 'prog-mode-hook #'emacs-solo/eglot-setup)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))

  :bind (:map
         eglot-mode-map
         ("C-c l a" . eglot-code-actions)
         ("C-c l o" . eglot-code-actions-organize-imports)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format)))

;;; FLYMAKE
(use-package flymake
  :ensure nil
  :defer t
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-8" . flymake-goto-next-error)
              ("M-7" . flymake-goto-prev-error)
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! t" . toggle-flymake-diagnostics-at-eol))
  :custom
  (flymake-show-diagnostics-at-end-of-line nil)
  ;; (flymake-show-diagnostics-at-end-of-line 'short)
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string
   `((error "!" compilation-error)      ;; Alternatives: », E, W, i, !, ?)
     (warning "?" compilation-warning)
     (note "i" compilation-info)))
  :config
  ;; Define the toggle function
  (defun toggle-flymake-diagnostics-at-eol ()
    "Toggle the display of Flymake diagnostics at the end of the line
and restart Flymake to apply the changes."
    (interactive)
    (setq flymake-show-diagnostics-at-end-of-line
          (not flymake-show-diagnostics-at-end-of-line))
    (flymake-mode -1) ;; Disable Flymake
    (flymake-mode 1)  ;; Re-enable Flymake
    (message "Flymake diagnostics at end of line: %s"
             (if flymake-show-diagnostics-at-end-of-line
                 "Enabled" "Disabled"))))


;;; WHITESPACE
(use-package whitespace
  :ensure nil
  :defer t
  :hook (before-save . whitespace-cleanup)
  ;; if we wanna remove this hook at any time, eval:
  ;; (remove-hook 'before-save-hook #'whitespace-cleanup)
  )


;;; MAN
(use-package man
  :ensure nil
  :commands (man)
  :config
  (setq Man-notify-method 'pushy)) ; does not obey `display-buffer-alist'


;;; MINIBUFFER
(use-package minibuffer
  :ensure nil
  :custom
  (completion-styles '(partial-completion flex initials))
  (completions-format 'vertical)
  (completion-ignore-case t)
  (completion-show-help t)
  ;; (completion-auto-select t) ;; only turn this on if not using icomplete
  (enable-recursive-minibuffers t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  :config
  ;; Keep the cursor out of the read-only portions of the.minibuffer
  (setq minibuffer-prompt-properties
        '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Keep minibuffer lines unwrapped, long lines like on M-y will be truncated
  (add-hook 'minibuffer-setup-hook
          (lambda () (setq truncate-lines t)))

  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))


;;; NEWSTICKER
(use-package newsticker
  :ensure nil
  :defer t
  :custom
  (newsticker-treeview-treewindow-width 40)
  :hook
  (newsticker-treeview-item-mode . (lambda ()
                                     (define-key newsticker-treeview-item-mode-map
                                                 (kbd "V")
                                                 'emacs-solo/newsticker-play-yt-video-from-buffer)))
  :init
  (defun emacs-solo/newsticker-play-yt-video-from-buffer ()
    "Plays with mpv async, the current buffer found '* videoId: '."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\* videoId: \\(\\w+\\)" nil t)
        (let ((video-id (match-string 1)))
          (start-process "mpv-video" nil "mpv" (format "https://www.youtube.com/watch?v=%s" video-id))
          (message "Playing with mpv: %s" video-id))))))


;;; ELEC_PAIR
(use-package elec-pair
  :ensure nil
  :defer
  :hook (after-init . electric-pair-mode))

;;; PAREN
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-style 'mixed)
  (show-paren-context-when-offscreen t)) ;; show matches within window splits

;;; PROCED
(use-package proced
  :ensure nil
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-descent t)
  (proced-filter 'user) ;; We can change interactively with `s'
  :config
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))

;;; ORG
(use-package org
  :ensure nil
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq
   ;; Start collapsed for speed
   org-startup-folded t

   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  ;; Ellipsis styling
  (setq org-ellipsis " ▼ ")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil))


;;; TIME
(use-package time
  :ensure nil
  ;; :hook (after-init . display-time-mode) ;; If we'd like to see it on the modeline
  :custom
  (world-clock-time-format "%A %d %B %r %Z")
  (display-time-day-and-date t)
  (display-time-default-load-average nil)
  (display-time-mail-string "")
  (zoneinfo-style-world-list                ; use `M-x worldclock RET' to see it
   '(("America/Los_Angeles" "Los Angeles")
     ("America/New_York" "New York")
     ("UTC" "UTC")
     ("Europe/London" "London")
     ("Europe/Berlin" "Berlin")
     ("Europe/Athens" "Athens")
     ("Asia/Jerusalem" "Jerusalem")
     ("Asia/Riyadh" "Riyadh")
     ("Asia/Tehran" "Tehran")
     ("Asia/Kolkata" "Kolkata")
     ("Asia/Singapore" "Singapore")
     ("Asia/Shanghai" "Shanghai")
     ("Asia/Tokyo" "Tokyo")
     ("Australia/Sydney" "Sydney")
     ("Pacific/Auckland" "Auckland"))))


;;; UNIQUIFY
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;;; WHICH-KEY
(use-package which-key
  :defer t
  :ensure nil
  :hook
  (after-init . which-key-mode)
  :config
  (setq which-key-separator "  ")
  (setq which-key-prefix-prefix "... ")
  (setq which-key-max-display-columns 3)
  (setq which-key-idle-delay 1.5)
  (setq which-key-idle-secondary-delay 0.25)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-description-length 40))

;;; WEBJUMP
(use-package webjump
  :defer t
  :ensure nil
  :bind ("C-x /" . webjump)
  :custom
  (webjump-sites
   '(("DuckDuckGo" . [simple-query "www.duckduckgo.com" "www.duckduckgo.com/?q=" ""])
     ("Google" . [simple-query "www.google.com" "www.google.com/search?q=" ""])
     ("YouTube" . [simple-query "www.youtube.com/feed/subscriptions" "www.youtube.com/rnesults?search_query=" ""])
     ("ChatGPT" . [simple-query "https://chatgpt.com" "https://chatgpt.com/?q=" ""]))))


;;; -------------------- NON TREESITTER AREA
;;; SASS-MODE
(use-package scss-mode
  :mode "\\.sass\\'"
  :defer t)

;;; -------------------- TREESITTER AREA
;;; RUBY-TS-MODE
(use-package ruby-ts-mode
  :ensure nil
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :custom
  (add-to-list 'treesit-language-source-alist '(ruby "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src"))
  (ruby-indent-level 2)
  (ruby-indent-tabs-mode nil))

;;; JS-TS-MODE
(use-package js-ts-mode
  :ensure js ;; I care about js-base-mode but it is locked behind the feature "js"
  :mode "\\.jsx?\\'"
  :defer 't
  :custom
  (js-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
  (add-to-list 'treesit-language-source-alist '(jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "master" "src")))

;;; TYPESCRIPT-TS-MODE
(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :defer 't
  :custom
  (typescript-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
  (unbind-key "M-." typescript-ts-base-mode-map))

;;; TYPESCRIPT-TS-MODE
(use-package tsx-ts-mode
  :mode "\\.tsx\\'"
  :defer 't
  :custom
  (typescript-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
  (unbind-key "M-." typescript-ts-base-mode-map))


;;; RUST-TS-MODE
(use-package rust-ts-mode
  :ensure rust-ts-mode
  :mode "\\.rs\\'"
  :defer 't
  :custom
  (rust-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(rust "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")))

;;; TOML-TS-MODE
(use-package toml-ts-mode
  :ensure toml-ts-mode
  :mode "\\.toml\\'"
  :defer 't
  :config
  (add-to-list 'treesit-language-source-alist '(toml "https://github.com/ikatyang/tree-sitter-toml" "master" "src")))

;;; MARKDOWN-TS-MODE
(use-package markdown-ts-mode
  :ensure nil
  :mode "\\.md\\'"
  :defer 't
  :config
  (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode))
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

;;; YAML-TS-MODE
(use-package yaml-ts-mode
  :ensure yaml-ts-mode
  :mode "\\.yml\\'"
  :defer 't
  :config
  (add-to-list 'treesit-language-source-alist '(yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "master" "src")))

;;; DOCKERFILE-TS-MODE
(use-package dockerfile-ts-mode
  :ensure dockerfile-ts-mode
  :mode "\\Dockerfile.*\\'"
  :defer 't
  :config
  (add-to-list 'treesit-language-source-alist '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")))


;;; ------------------- EMACS-SOLO CUSTOMS
;;; EMACS-SOLO-HOOKS
;;
(use-package emacs-solo-hooks
  :ensure nil
  :no-require t
  :defer t
  :init

  (defun emacs-solo/prefer-tabs ()
    "Disables indent-tabs-mode, and prefer spaces over tabs."
    (interactive)
    (indent-tabs-mode -1))

  (add-hook 'prog-mode-hook #'emacs-solo/prefer-tabs))


;;; EMACS-SOLO-MOVEMENTS
;;
;;  Functions to better move around text and Emacs
;;
(use-package emacs-solo-movements
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/rename-buffer-and-move-to-new-window ()
    "Promotes a side buffer to a new window."
    (interactive)
    (let ((temp-name (make-temp-name "temp-buffer-")))
      (rename-buffer temp-name t)
      (delete-window)
      (split-window-right)
      (switch-to-buffer temp-name)))

  (global-set-key (kbd "C-x x x") 'emacs-solo/rename-buffer-and-move-to-new-window)


  (defun emacs-solo-movements/scroll-down-centralize ()
    (interactive)
    (scroll-up-command)
    (recenter))

  (defun emacs-solo-movements/scroll-up-centralize ()
    (interactive)
    (scroll-down-command)
    (unless (= (window-start) (point-min))
      (recenter))
    (when (= (window-start) (point-min))
      (let ((midpoint (/ (window-height) 2)))
        (goto-char (window-start))
        (forward-line midpoint)
        (recenter midpoint))))

  (global-set-key (kbd "C-v") #'emacs-solo-movements/scroll-down-centralize)
  (global-set-key (kbd "M-v") #'emacs-solo-movements/scroll-up-centralize)


  (defun emacs-solo-movements/format-current-file ()
    "Format the current file using biome if biome.json is present; otherwise, use prettier.
Also first tries the local node_modules/.bin and later the global bin."
    (interactive)
    (let* ((file (buffer-file-name))
           (project-root (locate-dominating-file file "node_modules"))
           (biome-config (and project-root (file-exists-p (expand-file-name "biome.json" project-root))))
           (local-biome (and project-root (expand-file-name "node_modules/.bin/biome" project-root)))
           (global-biome (executable-find "biome"))
           (local-prettier (and project-root (expand-file-name "node_modules/.bin/prettier" project-root)))
           (global-prettier (executable-find "prettier"))
           (formatter nil)
           (source nil)
           (command nil)
           (start-time (float-time))) ;; Capture the start time
      (cond
       ;; Use Biome if biome.json exists
       ((and biome-config local-biome (file-executable-p local-biome))
        (setq formatter local-biome)
        (setq source "biome (local)")
        (setq command (format "%s format --write %s" formatter (shell-quote-argument file))))
       ((and biome-config global-biome)
        (setq formatter global-biome)
        (setq source "biome (global)")
        (setq command (format "%s format --write %s" formatter (shell-quote-argument file))))

       ;; Fall back to Prettier if no biome.json
       ((and local-prettier (file-executable-p local-prettier))
        (setq formatter local-prettier)
        (setq source "prettier (local)")
        (setq command (format "%s --write %s" formatter (shell-quote-argument file))))
       ((and global-prettier)
        (setq formatter global-prettier)
        (setq source "prettier (global)")
        (setq command (format "%s --write %s" formatter (shell-quote-argument file)))))
      (if command
          (progn
            (save-buffer)
            (shell-command command)
            (revert-buffer t t t)
            (let ((elapsed-time (* 1000 (- (float-time) start-time)))) ;; Calculate elapsed time in ms
              (message "Formatted with %s - %.2f ms" source elapsed-time)))
        (message "No formatter found (biome or prettier)"))))

  (global-set-key (kbd "C-c p") #'emacs-solo-movements/format-current-file)


  (defun emacs-solo/transpose-split ()
    "Transpose a horizontal split into a vertical split, or vice versa."
    (interactive)
    (if (> (length (window-list)) 2)
        (user-error "More than two windows present")
      (let* ((this-win (selected-window))
             (other-win (next-window))
             (this-buf (window-buffer this-win))
             (other-buf (window-buffer other-win))
             (this-edges (window-edges this-win))
             (other-edges (window-edges other-win))
             (this-left (car this-edges))
             (other-left (car other-edges))
             (split-horizontally (not (= this-left other-left))))
        (delete-other-windows)
        (if split-horizontally
            (split-window-vertically)
          (split-window-horizontally))
        (set-window-buffer (selected-window) this-buf)
        (set-window-buffer (next-window) other-buf)
        (select-window this-win))))

  (global-set-key (kbd "C-x 4 t") #'emacs-solo/transpose-split))


;; ;;; EMACS-SOLO-MODE-LINE
;; ;;
;; ;;  Customizations to the mode-line
;; ;;
;; (use-package emacs-solo-mode-line
;;   :ensure nil
;;   :no-require t
;;   :defer t
;;   :init
;;   ;; Shorten big branches names
;;   (defun emacs-solo/shorten-vc-mode (vc)
;;     "Shorten VC string to at most 20 characters.
;;  Replacing `Git-' with a branch symbol."
;;     (let* ((vc (replace-regexp-in-string "^ Git[:-]" "  " vc))) ;; Options:   ᚠ ⎇
;;       (if (> (length vc) 20)
;;           (concat (substring vc 0 20) "…")
;;         vc)))

;;   ;; Formats Modeline
;;   (setq-default mode-line-format
;;                 '("%e" "  "
;;                   ;; (:propertize " " display (raise +0.1)) ;; Top padding
;;                   ;; (:propertize " " display (raise -0.1)) ;; Bottom padding
;;                   (:propertize "λ  " face font-lock-keyword-face)

;;                   (:propertize
;;                    ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))

;;                   mode-line-frame-identification
;;                   mode-line-buffer-identification
;;                   "   "
;;                   mode-line-position
;;                   mode-line-format-right-align
;;                   "  "
;;                   (project-mode-line project-mode-line-format)
;;                   "  "
;;                   (vc-mode (:eval (emacs-solo/shorten-vc-mode vc-mode)))
;;                   "  "
;;                   mode-line-modes
;;                   mode-line-misc-info
;;                   "  ")
;;                 project-mode-line t
;;                 mode-line-buffer-identification '(" %b")
;;                 mode-line-position-column-line-format '(" %l:%c"))

;;   ;; Provides the Diminish functionality
;;   (defvar emacs-solo-hidden-minor-modes
;;     '(abbrev-mode
;;       eldoc-mode
;;       flyspell-mode
;;       smooth-scroll-mode
;;       outline-minor-mode
;;       which-key-mode))

;;   (defun emacs-solo/purge-minor-modes ()
;;     (interactive)
;;     (dolist (x emacs-solo-hidden-minor-modes nil)
;;       (let ((trg (cdr (assoc x minor-mode-alist))))
;;         (when trg
;;           (setcar trg "")))))

;;   (add-hook 'after-change-major-mode-hook 'emacs-solo/purge-minor-modes))


;;; EMACS-SOLO-EXEC-PATH-FROM-SHELL
;;
;;  Loads users default shell PATH settings into Emacs. Usefull
;;  when calling Emacs directly from GUI systems.
;;
(use-package emacs-solo-exec-path-from-shell
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment the same as user Shell."
    (interactive)
    (let ((path-from-shell
           (replace-regexp-in-string
            "[ \t\n]*$" "" (shell-command-to-string
                            "$SHELL --login -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))
      (message ">>> emacs-solo: PATH loaded")))

  (defun emacs-solo/fix-asdf-path ()
  "Ensure asdf shims and active Node.js version's bin directory are first in PATH."
  (interactive)
  (let* ((asdf-shims (expand-file-name "~/.asdf/shims"))
         (node-bin (string-trim (shell-command-to-string "asdf where nodejs 2>/dev/null")))
         (new-paths (list asdf-shims)))

    ;; If Node.js is installed, add its bin path
    (when (file-directory-p node-bin)
      (push (concat node-bin "/bin") new-paths))

    ;; Remove old asdf-related paths from PATH and exec-path
    (setq exec-path (seq-remove (lambda (p) (string-match-p "/\\.asdf/" p)) exec-path))
    (setenv "PATH" (string-join (seq-remove (lambda (p) (string-match-p "/\\.asdf/" p))
                                            (split-string (getenv "PATH") ":"))
                                ":"))

    ;; Add the new paths to exec-path and PATH
    (dolist (p (reverse new-paths))
      (unless (member p exec-path) (push p exec-path))
      (unless (member p (split-string (getenv "PATH") ":"))
        (setenv "PATH" (concat p ":" (getenv "PATH")))))))

  (add-hook 'find-file-hook #'emacs-solo/fix-asdf-path)
  (add-hook 'eshell-mode-hook #'emacs-solo/fix-asdf-path)
  (add-hook 'eshell-pre-command-hook #'emacs-solo/fix-asdf-path)
  (add-hook 'eshell-directory-change-hook #'emacs-solo/fix-asdf-path)

  (add-hook 'after-init-hook #'emacs-solo/set-exec-path-from-shell-PATH)
  (add-hook 'after-init-hook #'emacs-solo/fix-asdf-path))


;;; COLOR SCHEMES

;; Define the color schemes
(defconst almost-mono-themes-colors
  '((white . ((background . "#ffffff")
              (foreground . "#000000")
              (weak       . "#888888")
              (weaker     . "#dddddd")
              (weakest    . "#efefef")
              (highlight  . "#fda50f")
              (warning    . "#ff0000")
              (success    . "#00ff00")
              (string     . "#3c5e2b")))

    (black . ((background . "#000000")
              (foreground . "#ffffff")
              (weak       . "#aaaaaa")
              (weaker     . "#666666")
              (weakest    . "#222222")
              (highlight  . "#fda50f")
              (warning    . "#ff0000")
              (success    . "#00ff00")
              (string     . "#a7bca4")))

    (gray  . ((background . "#2b2b2b")
              (foreground . "#ffffff")
              (weak       . "#aaaaaa")
              (weaker     . "#666666")
              (weakest    . "#222222")
              (highlight  . "#fda50f")
              (warning    . "#ff0000")
              (success    . "#00ff00")
              (string     . "#a7bca4")))

    (cream . ((background . "#f0e5da")
              (foreground . "#000000")
              (weak       . "#7d7165")
              (weaker     . "#c4baaf")
              (weakest    . "#dbd0c5")
              (highlight  . "#fda50f")
              (warning    . "#ff0000")
              (success    . "#00ff00")
              (string     . "#3c5e2b")))))

;; Macro to bind colors
(defmacro almost-mono-themes--variant-with-colors (variant &rest body)
  `(let* ((colors (or (cdr (assoc ,variant almost-mono-themes-colors))
                      (error "No such theme variant")))
          (background (cdr (assoc 'background colors)))
          (foreground (cdr (assoc 'foreground colors)))
          (weak       (cdr (assoc 'weak colors)))
          (weaker     (cdr (assoc 'weaker colors)))
          (weakest    (cdr (assoc 'weakest colors)))
          (highlight  (cdr (assoc 'highlight colors)))
          (warning    (cdr (assoc 'warning colors)))
          (success    (cdr (assoc 'success colors)))
          (string     (cdr (assoc 'string colors))))
     ,@body))

;; Face specifications
(defmacro almost-mono-themes--faces-spec ()
  `(mapcar
    (lambda (entry) (list (car entry) `((t ,@(cdr entry)))))
    `(;; Your face definitions
      (default (:background ,background :foreground ,foreground))
      (fringe  (:background ,background))
      (region  (:background ,highlight  :foreground ,foreground))
      (show-paren-match (:background ,background :foreground ,success :bold t))
      (show-paren-mismatch (:background ,background :foreground ,warning :bold t))
      (minibuffer-prompt (:weight bold :foreground ,foreground))
      (isearch (:background ,weak :foreground ,foreground :bold t))
      (lazy-highlight (:background ,weaker :foreground ,foreground))
      (link (:underline t))
      (mode-line (:box (:line-width -1 :color ,weaker) :background ,weakest :foreground ,foreground))
      (mode-line-inactive (:box (:line-width -1 :color ,weaker) :background ,background :foreground ,weaker))
      (font-lock-keyword-face (:bold t))
      (font-lock-function-name-face (:bold t))
      (font-lock-variable-name-face (:foreground ,foreground))
      (font-lock-warning-face (:foreground ,foreground :underline (:color ,warning :style wave)))
      (font-lock-builtin-face (:bold t))
      (font-lock-constant-face (:bold t :italic t))
      (font-lock-type-face (:italic t))
      (font-lock-preprocessor-face (:italic t))
      (font-lock-comment-face (:foreground ,weak :italic t))
      (font-lock-string-face (:foreground ,string))
      (font-lock-doc-face (:inherit font-lock-comment-face))
      (line-number (:foreground ,weaker))
      (linum (:inherit line-number))
      (vertical-border (:foreground ,weaker))
      (eshell-prompt (:foreground ,foreground :bold t))
      (eshell-ls-directory (:foreground ,foreground :bold t))
      (eshell-ls-archive (:inherit eshell-ls-unreadable))
      (eshell-ls-backup (:inherit eshell-ls-unreadable))
      (eshell-ls-clutter (:inherit eshell-ls-unreadable))
      (eshell-ls-executable (:inherit eshell-ls-unreadable))
      (eshell-ls-missing (:inherit eshell-ls-unreadable))
      (eshell-ls-product (:inherit eshell-ls-unreadable))
      (eshell-ls-readonly (:inherit eshell-ls-unreadable))
      (eshell-ls-special (:inherit eshell-ls-unreadable))
      (eshell-ls-symlink (:inherit eshell-ls-unreadable))
      (company-tooltip (:background ,weakest :foreground ,foreground))
      (company-tooltip-selection (:background ,weaker :foreground ,foreground))
      (company-tooltip-common (:bold t))
      (company-tooltip-common-selection (:bold t))
      (company-scrollbar-bg (:background ,weaker))
      (company-scrollbar-fg (:background ,weak))
      (company-tooltip-annotation-selection (:background ,weaker :foreground ,foreground :italic t))
      (company-tooltip-annotation (:background ,weakest :foreground ,weak :italic t))
      (git-gutter:modified (:background ,highlight :foreground ,highlight))
      (git-gutter:added (:background ,success :foreground ,success))
      (git-gutter:deleted (:background ,warning :foreground ,warning))
      (diff-hl-change (:background ,highlight :foreground ,highlight))
      (diff-hl-insert (:background ,success :foreground ,success))
      (diff-hl-delete (:background ,warning :foreground ,warning))
      (hl-line (:background ,weakest))
      (highlight-current-line-face (:inherit hl-line))
      (ido-first-match (:bold t))
      (ido-only-match (:bold t))
      (ido-subdir (:italic t))
      (ido-virtual (:foreground ,weak))
      (ido-vertical-match-face (:bold t :italic nil))
      (org-table (:foreground ,weak)))))

;; Helper to create theme name
(defun almost-mono-themes--variant-name (variant)
  (intern (format "almost-mono-%s" (symbol-name variant))))

;; Define the theme programmatically
(defmacro almost-mono-themes--define-theme (variant)
  (let ((name (almost-mono-themes--variant-name variant))
        (doc (format "almost mono theme (%s version)" variant)))
    `(progn
       (deftheme ,name ,doc)
       (almost-mono-themes--variant-with-colors
        ',variant
        (apply 'custom-theme-set-faces ',name
               (almost-mono-themes--faces-spec)))
       (when (fboundp 'custom-theme-set-variables)
         (custom-theme-set-variables ',name))  ; Empty variables section
       (enable-theme ',name))))  ; Enable immediately after definition

;; Define all themes
(almost-mono-themes--define-theme white)
(almost-mono-themes--define-theme black)
(almost-mono-themes--define-theme gray)
(almost-mono-themes--define-theme cream)

;; Theme toggle functionality
(defvar almost-mono-current-theme 'white
  "The currently active almost-mono theme variant.")

(defconst almost-mono-theme-list '(white black gray cream)
  "List of available almost-mono theme variants.")

(defun almost-mono-cycle-theme ()
  "Cycle through the available almost-mono themes."
  (interactive)
  (let* ((current-idx (cl-position almost-mono-current-theme almost-mono-theme-list))
         (next-idx (mod (1+ current-idx) (length almost-mono-theme-list)))
         (next-theme (nth next-idx almost-mono-theme-list)))
    (disable-theme (almost-mono-themes--variant-name almost-mono-current-theme))
    (enable-theme (almost-mono-themes--variant-name next-theme))
    (setq almost-mono-current-theme next-theme)
    (message "Switched to %s theme" next-theme)))

;; Bind the cycle function to a key
(global-set-key (kbd "<f12>") 'almost-mono-cycle-theme)

;; Optional: Set initial theme explicitly if needed
(enable-theme 'almost-mono-white)

;;; EMACS-SOLO-RAINBOW-DELIMITERS
;;
;;  Colorizes matching delimiters
;;
;;  FIXME: Make it play nice with treesitter modes
;;
(use-package emacs-solo-rainbow-delimiters
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/rainbow-delimiters ()
    "Apply simple rainbow coloring to parentheses, brackets, and braces in the current buffer.
Opening and closing delimiters will have matching colors."
    (interactive)
    (let ((colors '((:foreground "#F2DFC4")
                    (:foreground "#F2C4F2")
                    (:foreground "#D4F2C4")
                    (:foreground "#C4E0F2"))))
      (font-lock-add-keywords
       nil
       `((,(rx (or "(" ")" "[" "]" "{" "}"))
          (0 (let* ((char (char-after (match-beginning 0)))
                    (depth (save-excursion
                             ;; Move to the correct position based on opening/closing delimiter
                             (if (member char '(?\) ?\] ?\}))
                                 (progn
                                   (backward-char) ;; Move to the opening delimiter
                                   (car (syntax-ppss)))
                               (car (syntax-ppss)))))
                    (face (nth (mod depth ,(length colors)) ',colors)))
               (list 'face face)))))))
    (font-lock-flush)
    (font-lock-ensure))

  (add-hook 'prog-mode-hook #'emacs-solo/rainbow-delimiters))

;;; EMACS-SOLO-PROJECT-SELECT
;;
;;  Interactively finds a project in a Projects folder and sets it
;;  to current `project.el' project.
;;
(use-package emacs-solo-project-select
  :ensure nil
  :no-require t
  :init
  (defvar emacs-solo-default-projects-folder "~/Projects"
    "Default folder to search for projects.")

  (defvar emacs-solo-default-projects-input "**"
    "Default input to use when finding a project.")

  (defun emacs-solo/find-projects-and-switch (&optional directory)
    "Find and switch to a project directory from ~/Projects."
    (interactive)
    (let* ((d (or directory emacs-solo-default-projects-folder))
           ;; (find-command (concat "fd --type d --max-depth 4 . " d))           ; with fd
           (find-command (concat "find " d " -mindepth 1 -maxdepth 4 -type d"))  ; with find
           (project-list (split-string (shell-command-to-string find-command) "\n" t))
           (initial-input emacs-solo-default-projects-input))
      (let ((selected-project
             (completing-read
              "Search project folder: "
              project-list
              nil nil
              initial-input)))
        (when (and selected-project (file-directory-p selected-project))
          (project-switch-project selected-project)))))

  (defun emacs-solo/minibuffer-move-cursor ()
    "Move cursor between `*` characters when minibuffer is populated with `**`."
    (when (string-prefix-p emacs-solo-default-projects-input (minibuffer-contents))
      (goto-char (+ (minibuffer-prompt-end) 1))))

  (add-hook 'minibuffer-setup-hook #'emacs-solo/minibuffer-move-cursor)

  :bind (:map project-prefix-map
         ("P" . emacs-solo/find-projects-and-switch)))


;;; EMACS-SOLO-HIGHLIGHT-KEYWORDS-MODE
;;
;;  Highlights a list of words like TODO, FIXME...
;;  Code borrowed from `alternateved'
;;
(use-package emacs-solo-highlight-keywords-mode
  :ensure nil
  :no-require t
  :defer t
  :init
  (defcustom +highlight-keywords-faces
    '(("TODO" . error)
      ("FIXME" . error)
      ("HACK" . warning)
      ("NOTE" . warning)
      ("HERE" . compilation-info))
    "Alist of keywords to highlight and their face."
    :group '+highlight-keywords
    :type '(alist :key-type (string :tag "Keyword")
                  :value-type (symbol :tag "Face"))
    :set (lambda (sym val)
           (dolist (face (mapcar #'cdr val))
             (unless (facep face)
               (error "Invalid face: %s" face)))
           (set-default sym val)))

  (defvar +highlight-keywords--keywords
    (when +highlight-keywords-faces
      (let ((keywords (mapcar #'car +highlight-keywords-faces)))
        `((,(regexp-opt keywords 'words)
           (0 (when (nth 8 (syntax-ppss))
                (cdr (assoc (match-string 0) +highlight-keywords-faces)))
              prepend)))))
    "Keywords and corresponding faces for `emacs-solo/highlight-keywords-mode'.")

  (defun emacs-solo/highlight-keywords-mode-on ()
    (font-lock-add-keywords nil +highlight-keywords--keywords t)
    (font-lock-flush))

  (defun emacs-solo/highlight-keywords-mode-off ()
    (font-lock-remove-keywords nil +highlight-keywords--keywords)
    (font-lock-flush))

  (define-minor-mode emacs-solo/highlight-keywords-mode
    "Highlight TODO and similar keywords in comments and strings."
    :lighter " +HL"
    :group '+highlight-keywords
    (if emacs-solo/highlight-keywords-mode
        (emacs-solo/highlight-keywords-mode-on)
      (emacs-solo/highlight-keywords-mode-off)))

  :hook
  (prog-mode . (lambda () (run-at-time "1 sec" nil #'emacs-solo/highlight-keywords-mode-on))))


;;; EMACS-SOLO-GUTTER
;;
;;  A **HIGHLY** `experimental' and slow and buggy git gutter like.
;;
(use-package emacs-solo-gutter
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/goto-next-hunk ()
    "Jump cursor to the closest next hunk."
    (interactive)
    (let* ((current-line (line-number-at-pos))
           (line-numbers (mapcar #'car git-gutter-diff-info))
           (sorted-line-numbers (sort line-numbers '<))
           (next-line-number
            (if (not (member current-line sorted-line-numbers))
                ;; If the current line is not in the list, find the next closest line number
                (cl-find-if (lambda (line) (> line current-line)) sorted-line-numbers)
              ;; If the current line is in the list, find the next line number that is not consecutive
              (let ((last-line nil))
                (cl-loop for line in sorted-line-numbers
                         when (and (> line current-line)
                                   (or (not last-line)
                                       (/= line (1+ last-line))))
                         return line
                         do (setq last-line line))))))

      (when next-line-number
        (goto-line next-line-number))))

  (defun emacs-solo/goto-previous-hunk ()
    "Jump cursor to the closest previous hunk."
    (interactive)
    (let* ((current-line (line-number-at-pos))
           (line-numbers (mapcar #'car git-gutter-diff-info))
           (sorted-line-numbers (sort line-numbers '<))
           (previous-line-number
            (if (not (member current-line sorted-line-numbers))
                ;; If the current line is not in the list, find the previous closest line number
                (cl-find-if (lambda (line) (< line current-line)) (reverse sorted-line-numbers))
              ;; If the current line is in the list, find the previous line number that has no direct predecessor
              (let ((previous-line nil))
                (dolist (line sorted-line-numbers)
                  (when (and (< line current-line)
                             (not (member (1- line) line-numbers)))
                    (setq previous-line line)))
                previous-line))))

      (when previous-line-number
        (goto-line previous-line-number))))


  (defun emacs-solo/git-gutter-process-git-diff ()
    "Process git diff for adds/mods/removals.
Marks lines as added, deleted, or changed."
    (interactive)
    (setq-local result '())
    (let* ((file-path (buffer-file-name))
           (grep-command "rg -Po")                         ; for rgrep
           ;; (grep-command (if (eq system-type 'darwin)   ; for grep / ggrep
           ;;                   "ggrep -Po"
           ;;                 "grep -Po"))
           (output (shell-command-to-string
                    (format
                     "git diff --unified=0 %s | %s '^@@ -[0-9]+(,[0-9]+)? \\+\\K[0-9]+(,[0-9]+)?(?= @@)'"
                     file-path
                     grep-command))))
      (setq-local lines (split-string output "\n"))
      (dolist (line lines)
        (if (string-match "\\(^[0-9]+\\),\\([0-9]+\\)\\(?:,0\\)?$" line)
            (let ((num (string-to-number (match-string 1 line)))
                  (count (string-to-number (match-string 2 line))))
              (if (= count 0)
                  (add-to-list 'result (cons (+ 1 num) "deleted"))
                (dotimes (i count)
                  (add-to-list 'result (cons (+ num i) "changed")))))
          (if (string-match "\\(^[0-9]+\\)$" line)
              (add-to-list 'result (cons (string-to-number line) "added"))))
        (setq-local git-gutter-diff-info result))
      result))


  (defun emacs-solo/git-gutter-add-mark (&rest args)
    "Add symbols to the left margin based on Git diff statuses.
   - '+' for added lines (lightgreen)
   - '~' for changed lines (yellowish)
   - '-' for deleted lines (tomato)."
    (interactive)
    (set-window-margins (selected-window) 2 0) ;; change to 1,2,3 if you want more columns
    (remove-overlays (point-min) (point-max) 'emacs-solo--git-gutter-overlay t)
    (let ((lines-status (or (emacs-solo/git-gutter-process-git-diff) '())))
      (save-excursion
        (dolist (line-status lines-status)
          (let ((line-num (car line-status))
                (status (cdr line-status)))
            (when (and line-num status)
              (goto-char (point-min))
              (forward-line (1- line-num))
              (let ((overlay (make-overlay (point-at-bol) (point-at-bol))))
                (overlay-put overlay 'emacs-solo--git-gutter-overlay t)
                (overlay-put overlay 'before-string
                             (propertize " "
                                         'display
                                         `((margin left-margin)
                                           ,(propertize
                                             (cond                              ;; Alternatives:
                                              ((string= status "added")   "│")  ;; +  │ ▏┃
                                              ((string= status "changed") "│")  ;; ~
                                              ((string= status "deleted") "│")) ;; _
                                             'face
                                             `(:foreground
                                               ,(cond
                                                 ((string= status "added") "lightgreen")
                                                 ((string= status "changed") "gold")
                                                 ((string= status "deleted") "tomato"))))))))))))))

  (defun emacs-solo/timed-git-gutter-on()
    (run-at-time 0.1 nil #'emacs-solo/git-gutter-add-mark))

  (defun emacs-solo/git-gutter-off ()
    "Remove all `emacs-solo--git-gutter-overlay' marks and other overlays."
    (interactive)
    (set-window-margins (selected-window) 2 0)
    (remove-overlays (point-min) (point-max) 'emacs-solo--git-gutter-overlay t)
    (remove-hook 'find-file-hook #'emacs-solo-git-gutter-on)
    (remove-hook 'after-save-hook #'emacs-solo/git-gutter-add-mark))

  (defun emacs-solo/git-gutter-on ()
    (interactive)
    (emacs-solo/git-gutter-add-mark)
    (add-hook 'find-file-hook #'emacs-solo/timed-git-gutter-on)
    (add-hook 'after-save-hook #'emacs-solo/git-gutter-add-mark))

  (global-set-key (kbd "M-9") 'emacs-solo/goto-previous-hunk)
  (global-set-key (kbd "M-0") 'emacs-solo/goto-next-hunk)
  (global-set-key (kbd "C-c g p") 'emacs-solo/goto-previous-hunk)
  (global-set-key (kbd "C-c g r") 'emacs-solo/git-gutter-off)
  (global-set-key (kbd "C-c g g") 'emacs-solo/git-gutter-on)
  (global-set-key (kbd "C-c g n") 'emacs-solo/goto-next-hunk)

  (add-hook 'after-init-hook #'emacs-solo/git-gutter-on))


;;; EMACS-SOLO-ACE-WINDOW
;;
;;  Based on: https://www.reddit.com/r/emacs/comments/1h0zjvq/comment/m0uy3bo/?context=3
;;
;;  TODO: implement ace-swap like feature
(use-package emacs-solo-ace-window
  :ensure nil
  :no-require t
  :defer t
  :init
  (defvar emacs-solo-ace-window/quick-window-overlays nil
    "List of overlays used to temporarily display window labels.")

  (defun emacs-solo-ace-window/quick-window-jump ()
    "Jump to a window by typing its assigned character label.
Windows are labeled starting from the top-left window and proceeding top to bottom, then left to right."
    (interactive)
    (let* ((window-list (emacs-solo-ace-window/get-windows))
           (window-keys (seq-take '("1" "2" "3" "4" "5" "6" "7" "8")
                                  (length window-list)))
           (window-map (cl-pairlis window-keys window-list)))
      (emacs-solo-ace-window/add-window-key-overlays window-map)
      (let ((key (read-key (format "Select window [%s]: " (string-join window-keys ", ")))))
        (emacs-solo-ace-window/remove-window-key-overlays)
        (if-let ((selected-window (cdr (assoc (char-to-string key) window-map))))
            (select-window selected-window)
          (message "No window assigned to key: %c" key)))))

  (defun emacs-solo-ace-window/get-windows ()
    "Return a list of windows in the current frame, ordered from top to bottom, left to right."
    (sort (window-list nil 'no-mini)
          (lambda (w1 w2)
            (let ((edges1 (window-edges w1))
                  (edges2 (window-edges w2)))
              (or (< (car edges1) (car edges2)) ; Compare top edges
                  (and (= (car edges1) (car edges2)) ; If equal, compare left edges
                       (< (cadr edges1) (cadr edges2))))))))

  (defun emacs-solo-ace-window/add-window-key-overlays (window-map)
    "Add temporary overlays to windows with their assigned key labels from WINDOW-MAP."
    (setq emacs-solo-ace-window/quick-window-overlays nil)
    (dolist (entry window-map)
      (let* ((key (car entry))
             (window (cdr entry))
             (start (window-start window))
             (overlay (make-overlay start start (window-buffer window))))
        (overlay-put overlay 'after-string
                     (propertize (format " [%s] " key)
                                 'face '(:foreground "#c3e88d"
                                         :background "#232635"
                                         :weight bold
                                         :height default)))
        (overlay-put overlay 'window window)
        (push overlay emacs-solo-ace-window/quick-window-overlays))))

  (defun emacs-solo-ace-window/remove-window-key-overlays ()
    "Remove all temporary overlays used to display key labels in windows."
    (mapc 'delete-overlay emacs-solo-ace-window/quick-window-overlays)
    (setq emacs-solo-ace-window/quick-window-overlays nil))

  (global-set-key (kbd "M-O") #'emacs-solo-ace-window/quick-window-jump))


;; ---------- EMACS-SOLO-OLIVETTI
(use-package emacs-solo-olivetti
  :ensure nil
  :no-require t
  :defer t
  :init
  (defvar emacs-solo-center-document-desired-width 90
    "The desired width of a document centered in the window.")

  (defun emacs-solo/center-document--adjust-margins ()
    ;; Reset margins first before recalculating
    (set-window-parameter nil 'min-margins nil)
    (set-window-margins nil nil)

    ;; Adjust margins if the mode is on
    (when emacs-solo/center-document-mode
      (let ((margin-width (max 0
                               (truncate
                                (/ (- (window-width)
                                      emacs-solo-center-document-desired-width)
                                   2.0)))))
        (when (> margin-width 0)
          (set-window-parameter nil 'min-margins '(0 . 0))
          (set-window-margins nil margin-width margin-width)))))

  (define-minor-mode emacs-solo/center-document-mode
    "Toggle centered text layout in the current buffer."
    :lighter " Centered"
    :group 'editing
    (if emacs-solo/center-document-mode
        (add-hook 'window-configuration-change-hook #'emacs-solo/center-document--adjust-margins 'append 'local)
      (remove-hook 'window-configuration-change-hook #'emacs-solo/center-document--adjust-margins 'local))
    (emacs-solo/center-document--adjust-margins))


  (add-hook 'org-mode-hook #'emacs-solo/center-document-mode)
  (add-hook 'gnus-group-mode-hook #'emacs-solo/center-document-mode)
  (add-hook 'gnus-summary-mode-hook #'emacs-solo/center-document-mode)
  (add-hook 'gnus-article-mode-hook #'emacs-solo/center-document-mode)

  ;; (add-hook 'newsticker-treeview-list-mode-hook 'emacs-solo/timed-center-visual-fill-on)
  ;; (add-hook 'newsticker-treeview-item-mode-hook 'emacs-solo/timed-center-visual-fill-on)
  )

;; ---------- EMACS-SOLO-0x0
;;
;; Inspired by: https://codeberg.org/daviwil/dotfiles/src/branch/master/Emacs.org#headline-28
(use-package emacs-solo-0x0
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/0x0-upload-text ()
    (interactive)
    (let* ((contents (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (buffer-string)))
           (temp-file (make-temp-file "0x0" nil ".txt" contents)))
      (message "Sending %s to 0x0.st..." temp-file)
      (let ((url (string-trim-right
                  (shell-command-to-string
                   (format "curl -s -F'file=@%s' https://0x0.st" temp-file)))))
        (message "The URL is %s" url)
        (kill-new url)
        (delete-file temp-file))))

  (defun emacs-solo/0x0-upload-file (file-path)
    (interactive "fSelect a file to upload: ")
    (message "Sending %s to 0x0.st..." file-path)
    (let ((url (string-trim-right
                (shell-command-to-string
                 (format "curl -s -F'file=@%s' https://0x0.st" (expand-file-name file-path))))))
      (message "The URL is %s" url)
      (kill-new url))))


;; ---------- EMACS-SOLO-SUDO-EDIT
;;
;; Inspired by: https://codeberg.org/daviwil/dotfiles/src/branch/master/Emacs.org#headline-28
(use-package emacs-solo-sudo-edit
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/sudo-edit (&optional arg)
    "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
    (interactive "P")
    (if (or arg (not buffer-file-name))
        (find-file (concat "/sudo:root@localhost:"
                           (completing-read "Find file(as root): ")))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))))


;; ---------- EMACS-SOLO-REPLACE-AS-DIFF
;;
(use-package emacs-solo/replace-regexp-as-diff
  :ensure nil
  :no-require t
  :defer t
  :init

  ;; NOTE: improvements wrappers over `multi-file-replace-regexp-as-diff', so
  ;;       we can:
  ;;       1.) Use it with glob pattern matching in files, including inside
  ;;           subfolders (`emacs-solo/multi-file-replace-regexp-as-diff-with-glob')
  ;;       2.) Use it with marked files and or directories in dired
  ;;           (`emacs-solo/dired-do-replace-regexp-as-diff')


  ;; `M-x emacs-solo/multi-file-replace-regexp-as-diff-with-glob RET'
  ;;
  ;; A wrapper for `multi-file-replace-regexp-as-diff' that extends its functionality
  ;; to support glob patterns for file matching. It recursively searches all files
  ;; in the specified directory (including subdirectories) that match the given glob
  ;; pattern (e.g., `*.js`), and displays the replacements as diffs in the
  ;; `*replace-diff*` buffer. This allows for easy review and application of changes
  ;; across multiple files.
  (defun emacs-solo/multi-file-replace-regexp-as-diff-with-glob (dir regexp to-string &optional delimited glob-pattern)
    "Wrapper for `multi-file-replace-regexp-as-diff` that accepts a directory and a glob pattern.
DIR is the directory to search recursively.
REGEXP is the regular expression to replace.
TO-STRING is the replacement string.
DELIMITED is an optional argument passed to `multi-file-replace-regexp-as-diff`.
GLOB-PATTERN is the glob pattern to match files (e.g., \"*.el\")."
    (interactive
     (let ((dir (file-truename (read-directory-name "Directory: ")))
           (common (query-replace-read-args
                    (concat "Replace"
                            (if current-prefix-arg " word" "")
                            " regexp as diff in files")
                    t t))
           (glob-pattern (read-string "Glob pattern (e.g., *.el): " "*")))
       (list dir (nth 0 common) (nth 1 common) (nth 2 common) glob-pattern)))

    (let* ((glob-regexp (wildcard-to-regexp glob-pattern))
           ;; file-expand-wildcards instead of directory-files-recursively, would
           ;; not allow us to traverse directories
           (files (directory-files-recursively dir glob-regexp)))

      (if files
          (multi-file-replace-regexp-as-diff files regexp to-string delimited)
        (message "No files found for glob-pattern: %s" glob-pattern))))


  ;; `M-x dired RET' mark files and/or directories then
  ;; `M-x emacs-solo/multi-file-replace-regexp-as-diff-with-glob RET'
  ;;
  ;; A version of `dired-do-replace-regexp-as-diff' that adds support for selected
  ;; directories in Dired. When directories are marked, it recursively includes all
  ;; files within them (and their subdirectories) in the replacement operation.
  ;; The replacements are displayed as diffs in the `*replace-diff*` buffer, allowing
  ;; for review and application of changes across multiple files and directories.
  (defun emacs-solo/expand-directories (items)
    "Expand ITEMS to include all files within directories (recursively).
Directories themselves are excluded from the final list."
    (cl-loop for item in items
             if (file-directory-p item)
             append (let ((files (directory-files-recursively item ".*" t)))
                      (cl-remove-if #'file-directory-p files))
             else if (file-regular-p item) ; Ensure only regular files are included
             collect item))
  (defun emacs-solo/dired-do-replace-regexp-as-diff (from to &optional delimited)
    "Do `replace-regexp' of FROM with TO as diff, on all marked files and directories.
If a marked item is a directory, all files within it (recursively) are included.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
The replacements are displayed in the buffer *replace-diff* that
you can later apply as a patch after reviewing the changes."
    (interactive
     (let ((common
            (query-replace-read-args
             "Replace regexp as diff in marked files and directories" t t t)))
       (list (nth 0 common) (nth 1 common) (nth 2 common))))
    (dired-post-do-command)
    (let* ((marked-items (dired-get-marked-files)) ; Include directories in the list
           (files (emacs-solo/expand-directories marked-items)))
      (if files
          (progn
            (multi-file-replace-regexp-as-diff files from to delimited))
        (message "No files found in marked items.")))))

(provide 'init)

;;; LIGATURES

(defvar ligature-composition-table
  '((prog-mode
     ("!" . "\\(\\!+\\|==\\|=<\\|=\\|≡+\\)")
     ("#" . "\\(_(\\|#+\\|[?[{_(]\\)") ; #_( ## #( #? #[ #_ #{
     ("$" . ">") ; $>
     ("%" . "=") ; %=
     ("&" . "\\(&+\\|[%+/=-]\\)") ; &&& && &% &+ &- &/ &=
     ("(" . "|") ; (|
     ("*" . "\\(\\*\\*+\\|>\\)") ; *** *>
     ("+" . "\\(+?[=+]\\|>\\)") ; ++ +++ += ++= +>
     ("-" . "\\(|[:>|]?\\|\\\\/\\|[->]?[->]\\|<[<|-]?\\|+-\\| \\[[xv -]\\]\\)") ; -|: -|> -| -|| -\/ -> --> ->- ->> -- --- -< -<- -<< -<| -+- - [v] - [x] - [-]
     ("." . "\\(\\.\\.?\\|=\\)") ; .. ... .=
     ("/" . "\\(/[/=]?\\|=[<:>=]?\\|-[<:>\\\\]\\)") ; // /// //= /-: /-> /-< /-\ /=< /=> /=: /== /=
     (":" . "\\(::?\\|-[/\\\\|]\\|=[/\\\\>|]?\\||[-=]\\|≡\\)") ; :: ::: :-/ :-\ :-| := :=/ :=> :=\ :=| :|- :|= :≡
     ("<" . "\\(<==\\|==>\\|=[/<=>\\]\\|<[<-=^|~]?\\|\\!--\\|\\*\\*>\\|-->?\\|-[/<\\\\]\\||[-|]\\|~[~<]\\|[\\!#$*%&+-./:?@^|~]?>\\|[$*+-=\\|~]\\)") ; << <<< <!-- <**> --> <--  <-/ <-< <-\ <!> <#> <$> <%> <&> <+> <-> <.> </> <:> <$ <* <+ <- <<= <<== <<^ <<| <<~ <= <=/ <=< <== <==> <=> <=> <=\ <> <?> <@> <\ <\> <^> <| <|- <|> <|| <~ <~< <~> <~~
     ("=" . "=*")
     ("?" . "\\(\\.\\|\\?+\\)")
     ("|" . "|+")
     ("a" . "\\(nd\\|uto\\)")
     ("b" . "\\(egin\\|reak\\)")
     ("c" . "\\(ase\\|hart?\\|onst\\|onst_cast\\|ontinue\\)")
     ("d" . "\\(efault\\|o\\|ouble\\)")
     ("e" . "\\(lse\\|num\\|xtern\\)")
     ("f" . "\\(allthrough\\|loat\\|or\\)")
     ("g" . "\\(o\\|oto\\)")
     ("i" . "\\(f\\|nt\\)")
     ("l" . "\\(et\\|ong\\)")
     ("p" . "\\(ackage\\|rivate\\|rotected\\|ublic\\)")
     ("r" . "\\(einterpret_cast\\|em\\|eturn\\)")
     ("s" . "\\(uper\\|ynchronize\\)")
     ("y" . "ield")
     ("[" . "\\(WARN ?\\|TODO\\)\\]") ; [TODO] [WARN] [WARN ]
     ("\\" . "\\\\+")
     ("↖︎" . "↗")))
  "A list of preprocessed ligature compositions.")

(defun ligature-generate-ligatures ()
  "Ligate the current buffer using its major mode to determine ligature set."
  (interactive)
  (let ((table (make-char-table nil)))
    (dolist (ligature-table ligature-composition-table)
      (let ((modes (car ligature-table))
            (rules (cdr ligature-table)))
        (when (or (eq modes t) (cl-some #'derived-mode-p (if (listp modes) modes (list modes))))
          (dolist (rule rules)
            (set-char-table-range table (string-to-char (car rule))
                                  `([,(concat "." (cdr rule)) 0 font-shape-gstring]))))))
    (set-char-table-parent table composition-function-table)
    (setq-local composition-function-table table)))

(define-minor-mode ligature-mode
  "Enables typographic ligatures."
  :init-value nil :lighter nil :keymap nil
  (if (not ligature-mode)
      (setq-local composition-function-table (default-value 'composition-function-table))
    (unless (memq major-mode '(minibuffer-inactive-mode))
      (ligature-generate-ligatures))))

(define-globalized-minor-mode global-ligature-mode ligature-mode
  (lambda () (ligature-mode t)))

;; Enable ligatures globally
(global-ligature-mode 1)

;;; init.el ends here
