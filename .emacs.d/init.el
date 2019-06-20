(require 'cl-lib)
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

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
(setq-default fill-column 80)

;; misc
(setq custom-file "~/.emacs.d/custom.el")
(setq default-directory "~/dev/projects")
(load custom-file 'noerror)

;; prevent issues with dired in macOS
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

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
(setq require-final-newline t)

(delete-selection-mode t)

;; start new windows maximized
(setq frame-resize-pixelwise t)
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
(setq-default js-indent-level 2)
(setq-default python-indent-offset 4)

(setq-default electric-indent-inhibit t)
(setq-default evil-shift-width 2)
(global-set-key (kbd "TAB") 'tab-to-tab-stop)
(global-set-key (kbd "<S-tab>") 'evil-shift-left)

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
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))

;; enable (relative) line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; best font
(set-face-attribute 'default nil
  :family "Monaco" :height 230 :weight 'normal)

;; best theme
;; (use-package rebecca-theme
;;   :ensure t)
;; (load-theme 'rebecca t)
(load-theme 'leuven t)

;; cursor options
(blink-cursor-mode 0)
;; (set-cursor-color "#FAD000")

;; best package name
(use-package evil-leader
  :ensure t)

(evil-leader/set-key
  "v" 'vc-annotate
  "c" 'avy-goto-char-2
  "g" 'magit-status
  "n" 'dired
  "j" (let ((map (make-sparse-keymap)))
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
(add-hook 'css-mode-hook '(lambda ()
                          (add-hook 'after-save-hook 'evil-indent)))

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

(add-hook 'text-mode-hook 'turn-on-auto-fill)

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

(define-key ivy-minibuffer-map (kbd "s-k") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "s-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-o") 'ivy-occur)
(define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)

(use-package flx
  :ensure t)

(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (counsel-rg . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))

(setq ivy-initial-inputs-alist nil)

(setq counsel-rg-base-command
      "rg -i -M 120 --no-heading --line-number --color never %s .")

(use-package magit
  :ensure t)

(use-package forge
  :ensure t
  :after magit)

(use-package evil-magit
  :ensure t)

(use-package git-link
  :ensure t)

(use-package diff-hl
  :ensure t)

(global-diff-hl-mode)
(diff-hl-margin-mode)
(diff-hl-flydiff-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

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

(use-package smartparens
  :ensure t)

(require 'smartparens-config)
(add-hook 'prog-mode-hook #'smartparens-mode)

(use-package ace-window
  :ensure t)

(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

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

(setq initial-major-mode 'org-mode)

(use-package yasnippet
  :ensure t)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'org-mode-hook #'yas-minor-mode)
(add-hook 'markdown-mode-hook #'yas-minor-mode)

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

(use-package simpleclip
  :ensure t)

(simpleclip-mode 1)
(setq select-enable-clipboard nil)

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((ruby . t)
                                 (js . t))))

(setq org-confirm-babel-evaluate nil)

(use-package neotree
  :ensure t)

(global-set-key [f8] 'neotree-toggle)

(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "o") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "j") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "k") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

(setq neo-theme 'ascii)

(add-hook 'org-mode-hook
          (lambda () (setq evil-auto-indent nil)))

(use-package github-browse-file
  :ensure t)

(evil-set-initial-state 'vc-annotate-mode 'normal)
(evil-define-key 'normal vc-annotate-mode-map (kbd "q") 'quit-window)
(evil-define-key 'normal vc-annotate-mode-map (kbd "a") 'vc-annotate-revision-previous-to-line)
(evil-define-key 'normal vc-annotate-mode-map (kbd "d") 'vc-annotate-show-diff-revision-at-line)
(evil-define-key 'normal vc-annotate-mode-map (kbd "=") 'vc-annotate-show-diff-revision-at-line)
(evil-define-key 'normal vc-annotate-mode-map (kbd "F") 'vc-annotate-find-revision-at-line)
(evil-define-key 'normal vc-annotate-mode-map (kbd "J") 'vc-annotate-revision-at-line)
(evil-define-key 'normal vc-annotate-mode-map (kbd "L") 'vc-annotate-show-log-revision-at-line)
(evil-define-key 'normal vc-annotate-mode-map (kbd "C-j") 'vc-annotate-next-revision)
(evil-define-key 'normal vc-annotate-mode-map (kbd "C-k") 'vc-annotate-prev-revision)
(evil-define-key 'normal vc-annotate-mode-map (kbd "W") 'vc-annotate-working-revision)
(evil-define-key 'normal vc-annotate-mode-map (kbd "A") 'vc-annotate-toggle-annotation-visibility)
(evil-define-key 'normal vc-annotate-mode-map (kbd "RET") 'vc-annotate-goto-line)

;; does the opposite of fill-region <M-q>
(use-package unfill
  :ensure t)

(use-package deadgrep
  :ensure t)

(use-package elscreen
  :ensure t)

(setq-default elscreen-tab-display-kill-screen nil)
(setq-default elscreen-tab-display-control nil)
(elscreen-start)

(global-set-key (kbd "s-n") 'elscreen-clone)
(global-set-key (kbd "s-w") 'elscreen-kill)
(global-set-key (kbd "s-{") 'elscreen-previous)
(global-set-key (kbd "s-}") 'elscreen-next)

(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-right)
(global-set-key (kbd "s-3") 'split-window-below)
(global-set-key (kbd "s-0") 'delete-window)

(global-set-key (kbd "s-g") 'minibuffer-keyboard-quit)

(global-set-key (kbd "s-t") 'neotree-toggle)
(global-set-key (kbd "s-f") 'swiper)
(global-set-key (kbd "s-F") 'deadgrep)
(global-set-key (kbd "s-b") 'ivy-switch-buffer)

(global-set-key (kbd "s-]") 'evil-shift-right-line)
(global-set-key (kbd "s-[") 'evil-shift-left-line)

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-P") 'projectile-command-map)
  (projectile-mode +1))

(setq projectile-completion-system 'ivy)

(global-set-key (kbd "s-p") 'projectile-find-file)

(use-package rails-log-mode
  :ensure t)
