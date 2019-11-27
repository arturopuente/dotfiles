;; call M-x package-install use-package
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

(require 'use-package)

(when (string-equal system-type "darwin")
  ;; prevent issues with dired in macOS
  (setq dired-use-ls-dired nil)

  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")

  ;; Don't make new frames when opening a new file with Emacs
  (setq ns-pop-up-frames nil)

  (put 'ns-print-buffer 'disabled t)
  (put 'suspend-frame 'disabled t))

(use-package exec-path-from-shell
  :ensure t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; delete the region when typing
(delete-selection-mode t)

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

;; enables M-delete to work as intended
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)

;; avoids cluttering system clipboard with evil-{yank/change/delete}
(use-package simpleclip
  :ensure t)

(simpleclip-mode 1)
(setq select-enable-clipboard nil)

;; does the opposite of fill-region <M-q>
(use-package unfill
  :ensure t)

(setq default-directory "~/dev/projects")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; ~/.emacs.d/init.el is a symlink
(setq vc-follow-symlinks t)

;; remove startup messages
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq ring-bell-function 'ignore)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

(setq-default word-wrap t)
(setq-default fill-column 80)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

;; Don't count two spaces after a period as the end of a sentence.
;; Just one space is needed.
(setq sentence-end-double-space nil)

(show-paren-mode t)

;; display a simplified view by default
(add-hook 'dired-mode-hook
  (lambda ()
    (dired-hide-details-mode)))

;; disable backups
(setq make-backup-files nil
      vc-make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      create-lockfiles nil)

;; start new windows maximized
(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; remove unused menu and tool bar
(tool-bar-mode 0)
(menu-bar-mode 0)

;; display current column in the status bar
(setq column-number-mode t)

;; enable (relative) line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; best font
(set-face-attribute 'default nil
                    :family "Ubuntu Mono" :height 150 :weight 'normal)

(when (string-equal system-type "darwin")
  (set-face-attribute 'default nil
                      :family "Monaco" :height 170 :weight 'normal))

(load-theme 'leuven t)

;; cursor options
(blink-cursor-mode 0)

(use-package minions
  :ensure t
  :config (minions-mode 1))

;; configure indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default python-indent-offset 4)

(setq-default evil-shift-width 2)
(global-set-key (kbd "TAB") 'tab-to-tab-stop)
(global-set-key (kbd "<S-tab>") 'evil-shift-left)
(global-set-key (kbd "s-]") 'evil-shift-right-line)
(global-set-key (kbd "s-[") 'evil-shift-left-line)

;; best vim emulation mode
(use-package evil
  :ensure t)

(evil-mode 1)
(define-key evil-normal-state-map (kbd "C-f") 'evil-scroll-up)

;; don't move back the cursor one space after exiting evil edit mode
(setq evil-move-cursor-back nil)

;; best package name
(use-package evil-leader
  :ensure t)

(evil-leader/set-key
  "1" 'delete-other-windows
  "2" 'split-right-and-switch
  "3" 'split-below-and-switch
  "9" 'winner-undo
  "0" 'winner-redo
  "-" 'delete-window
  "o" 'ace-window
  "t" 'neotree-toggle
  "v" 'vc-annotate
  "c" 'avy-goto-char-2
  "g" 'magit-status
  "n" 'dired
  "j" (let ((map (make-sparse-keymap)))
        (define-key map (kbd "t") 'jest-popup)
        (define-key map (kbd "j") 'tide-jump-to-definition)
        (define-key map (kbd "b") 'tide-jump-back)
        map)
  )

(global-evil-leader-mode t)
(evil-leader/set-leader "<SPC>")

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-multiedit
  :ensure t)

(define-key evil-normal-state-map (kbd "s-d") 'evil-multiedit-match-and-next)
(define-key evil-normal-state-map (kbd "s-D") 'evil-multiedit-match-and-prev)

;; RET will toggle the region under the cursor
(define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

;; For moving between edit regions
(define-key evil-multiedit-state-map (kbd "s-j") 'evil-multiedit-next)
(define-key evil-multiedit-state-map (kbd "s-k") 'evil-multiedit-prev)

(evil-set-initial-state 'occur-mode 'normal)

(define-key occur-mode-map (kbd "C-x C-q") 'occur-edit-mode)
(define-key occur-mode-map (kbd "C-c C-c") 'occur-mode-goto-occurrence)

(define-key occur-edit-mode-map (kbd "C-x C-q") 'occur-cease-edit)
(define-key occur-edit-mode-map (kbd "C-c C-c") 'occur-cease-edit)

(global-set-key (kbd "C-c o") 'occur)

(use-package expand-region
  :ensure t)

(global-set-key (kbd "C-=") 'er/expand-region)

(use-package helpful
  :ensure t)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(use-package counsel
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
    (global-set-key (kbd "C-x l") 'counsel-locate)
  ))

(global-set-key (kbd "s-f") 'swiper)
(define-key ivy-minibuffer-map (kbd "s-k") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "s-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-o") 'ivy-occur)
(define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)

(evil-set-initial-state 'ivy-occur-grep-mode 'normal)

(use-package flx
  :ensure t)

(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))

(setq ivy-initial-inputs-alist nil)

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (ace-window-display-mode)
  :bind ("s-o" . ace-window))

(bind-key "M-s-o" 'ace-swap-window)

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode 1)
  :bind (("C-x u" . undo)))

;; jump like vim easymotion
(use-package avy
  :ensure t)

(use-package web-mode
  :ensure t
  :init (setq web-mode-markup-indent-offset 2)
        (setq web-mode-code-indent-offset 2)
        (setq web-mode-css-indent-offset 2)
        (setq web-mode-enable-auto-pairing t)
        (setq web-mode-enable-auto-closing t)
        (setq web-mode-enable-css-colorization t))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))

(use-package inf-ruby
  :ensure t)

(use-package robe
  :ensure t
  :hook (ruby-mode . robe-mode))

(use-package chruby
  :ensure t)

(add-hook 'ruby-mode-hook #'chruby-use-corresponding)

(use-package yaml-mode
  :ensure t)

(use-package rails-log-mode
  :ensure t)

(use-package python-pytest
  :ensure t)

;; better support for jsx
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

(use-package tide
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (company-mode +1))

(add-hook 'rjsx-mode-hook #'setup-tide-mode)

(use-package jest
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
        ("\\.md\\'" . markdown-mode)
        ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package org
  :ensure t
  :pin org)

(setq initial-major-mode 'org-mode)

(add-hook 'org-mode-hook
          (lambda () (setq evil-auto-indent nil)))

(use-package ob-restclient
  :ensure t)

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages
                                '((ruby . t)
                                  (calc . t)
                                  (python . t)
                                  (lisp . t)
                                  (shell . t)
                                  (restclient . t)
                                  (js . t))))

(setq org-confirm-babel-evaluate nil)

(setq org-src-fontify-natively t
      org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t)

(use-package magit
  :ensure t)

(setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

(use-package forge
  :ensure t
  :after magit)

(use-package evil-magit
  :ensure t)

(use-package git-link
  :ensure t)

(use-package diff-hl
  :ensure t)

(if (display-graphic-p)
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
)

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

(use-package restclient
  :ensure t)

(use-package edit-indirect
  :ensure t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(electric-pair-mode 1)

(use-package deadgrep
  :ensure t)

(global-set-key (kbd "s-F") 'deadgrep)

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-P") 'projectile-command-map)
  (projectile-mode +1))

(setq projectile-completion-system 'ivy)

(global-set-key (kbd "s-p") 'projectile-find-file)

(use-package flyspell
  :hook ((text-mode . flyspell-mode)
        (python-mode . flyspell-prog-mode)
        (ruby-mode . flyspell-prog-mode)
        (web-mode . flyspell-prog-mode)))

(setq ispell-program-name "/usr/local/bin/ispell")

(use-package company
  :ensure t
  :pin melpa)

(setq company-lsp-cache-candidates 'auto)
(add-hook 'after-init-hook 'global-company-mode)

(use-package which-key
  :ensure t)

(which-key-mode)

(use-package yasnippet
  :ensure t)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'org-mode-hook #'yas-minor-mode)
(add-hook 'markdown-mode-hook #'yas-minor-mode)

(use-package neotree
  :ensure t)

(global-set-key (kbd "s-t") 'neotree-toggle)

(setq neo-theme 'ascii)

(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "o") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "j") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "k") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

(defun split-right-and-switch ()
  "Splits the window vertically and switches to that window."
  (interactive)
  (split-window-right)
  (other-window 1 nil))

(defun split-below-and-switch ()
  "Splits the window horizontally and switches to that window."
  (interactive)
  (split-window-below)
  (other-window 1 nil))

(when (fboundp 'winner-mode)
      (winner-mode 1))

(bind-key "s-9" 'winner-undo)
(bind-key "s-0" 'winner-redo)

(bind-key "s-b" 'ivy-switch-buffer)
(bind-key "s-g" 'minibuffer-keyboard-quit)

;; buffer switching
(bind-key "s-{" 'previous-buffer)
(bind-key "s-}" 'next-buffer)
