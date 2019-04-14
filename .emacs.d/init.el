(require 'cl-lib)
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(require 'use-package)

(use-package exec-path-from-shell
  :ensure t)

;; Enable M-x without meta (needed for iPad/SSH access actually!)
(global-set-key "\C-x\C-m" 'execute-extended-command)

;; when not called from inside a terminal, GUI emacs does not set the
;; PATH or other environment variables correctly
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; default to UTF-8
(prefer-coding-system 'utf-8)

(setq-default word-wrap t)

;; misc
(setq custom-file "~/.emacs.d/custom.el")
(setq default-directory "~/dev/projects")
(load custom-file 'noerror)

;; prevent issues with dired in macOS
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

(add-hook 'dired-mode-hook
  (lambda ()
    (dired-hide-details-mode)))

(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")

;; disable backups
(setq make-backup-files nil
      vc-make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; sets fish as the command for ansi-term
(setq explicit-shell-file-name "/usr/local/bin/fish")

;; ~/.emacs.d/init.el is a symlink
(setq vc-follow-symlinks t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(delete-selection-mode t)

;; start new windows maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; remove unused menu and tool bar
(tool-bar-mode 0)
(menu-bar-mode 0)

;; display current column in the status bar
(setq column-number-mode t)

;; remove startup stuff
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq ring-bell-function 'ignore)

;; configure indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq js-indent-level 2)

(use-package web-mode
  :ensure t
  :init (setq web-mode-markup-indent-offset 2)
        (setq web-mode-code-indent-offset 2)
        (setq web-mode-css-indent-offset 2)
        (setq web-mode-enable-auto-pairing t)
        (setq web-mode-enable-auto-expanding t)
        (setq web-mode-enable-css-colorization t))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))

;; enable (relative) line numbers
(global-linum-mode t)

(use-package linum-relative
  :ensure t)

(linum-relative-mode t)
(setq linum-relative-current-symbol "")

;; best font
(set-face-attribute 'default nil
  :family "Monaco" :height 230 :weight 'normal)

;; best theme
(use-package rebecca-theme
  :ensure t)
(load-theme 'rebecca t)

;; cursor options
(blink-cursor-mode 0)
(set-cursor-color "#FAD000")

;; best package name
(use-package evil-leader
  :ensure t)

(evil-leader/set-key
  "s" 'swiper
  "j" 'avy-goto-char-2
  "g" 'magit-status
  "f" 'counsel-git
  "r" 'counsel-rg
  "n" 'dired
  "b" 'ivy-switch-buffer
  "t" (let ((map (make-sparse-keymap)))
        (define-key map (kbd "j") 'tide-jump-to-definition)
        (define-key map (kbd "b") 'tide-jump-back)
        map)
  )

(global-evil-leader-mode t)
(evil-leader/set-leader "<SPC>")

;; best vim emulation mode
(use-package evil
  :ensure t)

(evil-mode 1)
(define-key evil-normal-state-map (kbd "C-f") 'evil-scroll-up)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package expand-region
  :ensure t)

(global-set-key (kbd "C-=") 'er/expand-region)

;; don't move back the cursor one space after exiting evil edit mode
(setq evil-move-cursor-back nil)

;; better support for jsx and js in general
(use-package rjsx-mode
  :ensure t
  :mode "\\.jsx\\'"
  :mode "\\.js\\'")

(use-package prettier-js
  :ensure t)

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)

(use-package json-mode
  :ensure t
  :config (setq json-reformat:indent-width 2))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package edit-indirect
  :ensure t)

(use-package minions
  :ensure t
  :config (minions-mode 1))

(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(setq ispell-program-name "/usr/local/bin/ispell")

(use-package helpful
  :ensure t)

(use-package swiper
  :ensure t
  :after (helpful)
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "C-h f") 'counsel-describe-function)
    (global-set-key (kbd "C-h v") 'counsel-describe-variable)
    (global-set-key (kbd "C-x l") 'counsel-locate)
  ))

(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-o") 'ivy-occur)
(define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)

(use-package flx
  :ensure t)

(setq ivy-re-builders-alist
      '((ivy-switch-buffer . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))

(setq ivy-initial-inputs-alist nil)

(use-package magit
  :ensure t
  :bind ("C-c s" . 'magit))

(use-package evil-magit
  :ensure t)

(use-package git-link
  :ensure t)

(use-package ruby-electric
  :ensure t
  :hook (ruby-mode . ruby-electric-mode))

(electric-pair-mode t)

(use-package company
  :ensure t
  :pin melpa)

(add-hook 'after-init-hook 'global-company-mode)

(use-package inf-ruby
  :ensure t)

(use-package robe
  :ensure t
  :hook (ruby-mode . robe-mode))

(use-package chruby
  :ensure t)

(chruby "ruby-2.6")

(use-package tide
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(add-hook 'rjsx-mode-hook #'setup-tide-mode)

(use-package web-mode
  :ensure t
  :mode "\\.erb\\'")

(use-package yaml-mode
  :ensure t)

(use-package ace-window
  :ensure t)

(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode 1)
  :bind (("C-x u" . undo)))

;; jump like vim easymotion
(use-package avy
  :ensure t)

(use-package restclient
  :ensure t)

(use-package evil-mc
  :ensure t
  :bind (
    :map evil-mc-key-map
         ("C-g" . evil-mc-undo-all-cursors)
         ("C-j" . evil-mc-make-and-goto-next-match)
         ("C-k" . evil-mc-make-and-goto-prev-match)
    )
  )

(use-package which-key
  :ensure t)

(which-key-mode)

(use-package fish-mode
  :ensure t)

(use-package elixir-mode
  :ensure t)

(use-package alchemist
  :ensure t)

(use-package org
  :ensure t
  :pin org)

(use-package yasnippet
  :ensure t)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)

(evil-define-operator clip-evil-delete (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (unless register
    (let ((text (filter-buffer-substring beg end)))
      (unless (string-match-p "\n" text)
        ;; set the small delete register
        (evil-set-register ?- text))))
  (cond
   ((eq type 'block)
    (evil-apply-on-block #'delete-region beg end nil))
   ((and (eq type 'line)
         (= end (point-max))
         (or (= beg end)
             (/= (char-before end) ?\n))
         (/= beg (point-min))
         (=  (char-before beg) ?\n))
    (delete-region (1- beg) end))
   (t
    (delete-region beg end)))
  ;; place cursor on beginning of line
  (when (and (called-interactively-p 'any)
             (eq type 'line))
    (evil-first-non-blank)))

(evil-define-operator clip-evil-change
  (beg end type register yank-handler delete-func)
  "Change text from BEG to END with TYPE.
Save in REGISTER or the kill-ring with YANK-HANDLER.
DELETE-FUNC is a function for deleting text, default `evil-delete'.
If TYPE is `line', insertion starts on an empty line.
If TYPE is `block', the inserted text in inserted at each line
of the block."
  (interactive "<R><x><y>")
  (let ((delete-func (or delete-func #'clip-evil-delete))
        (nlines (1+ (evil-count-lines beg end)))
        (opoint (save-excursion
                  (goto-char beg)
                  (line-beginning-position))))
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step))
    (funcall delete-func beg end type register yank-handler)
    (cond
     ((eq type 'line)
      (if ( = opoint (point))
          (evil-open-above 1)
        (evil-open-below 1)))
     ((eq type 'block)
      (evil-insert 1 nlines))
     (t
      (evil-insert 1)))))

(define-key evil-normal-state-map "c" 'clip-evil-change)
