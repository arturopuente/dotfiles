(require 'cl-lib)
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(require 'use-package)

;; yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; default to UTF-8
(prefer-coding-system 'utf-8)

;; misc
(setq custom-file "~/.emacs.d/custom.el")
(setq default-directory "~/dev/projects")
(load custom-file 'noerror)

;; prevent issues with dired in macOS
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

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

;; start new windows maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; remove unused menu and tool bar
(tool-bar-mode 0)
(menu-bar-mode 0)

;; display current column in the status bar
(setq column-number-mode t)

;; remove cursor blink
(blink-cursor-mode 0)

;; remove startup stuff
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq ring-bell-function 'ignore)

;; configure indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq js-indent-level 2)

(defun arturo/web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)

(add-hook 'web-mode-hook  'arturo/web-mode-hook)

;; enable (relative) line numbers
(global-linum-mode t)

(use-package linum-relative
  :ensure t)

(linum-relative-mode t)

;; best font
(set-face-attribute 'default nil
  :family "Monaco" :height 230 :weight 'normal)

;; best theme
(use-package rebecca-theme
  :ensure t)
(load-theme 'rebecca t)

;; best package name
(use-package evil-leader
  :ensure t)

(evil-leader/set-key
  "s" 'swiper
  "j" 'avy-goto-char
  "g" 'magit-status
  "f" 'counsel-git
  "d" 'counsel-git-grep
  "b" 'ivy-switch-buffer)

(global-evil-leader-mode t)
(evil-leader/set-leader "<SPC>")

;; best vim emulation mode
(use-package evil
  :ensure t)

(evil-mode 1)

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

;; hook prettier to run in the major web modes
(use-package prettier-js
  :ensure t)

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)

(use-package json-mode
  :ensure t
  :config (setq json-reformat:indent-width 2))

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
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "C-h f") 'counsel-describe-function)
    (global-set-key (kbd "C-h v") 'counsel-describe-variable)
    (global-set-key (kbd "C-x p") 'counsel-git)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c f") 'counsel-git-grep)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  ))

(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-:") 'ivy-dired)
(define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur)

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

(add-hook 'rjsx-mode-hook 'electric-pair-mode)

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
