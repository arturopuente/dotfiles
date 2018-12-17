(require 'cl-lib)
(require 'package)

(let ((melpa '("melpa" . "https://melpa.org/packages/"))
      (org-mode '("org" . "https://orgmode.org/elpa/")))
  (add-to-list 'package-archives melpa t)
  (add-to-list 'package-archives org-mode t)
)

(require 'use-package)

;; misc
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; default window options
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(global-linum-mode t)
(setq column-number-mode t)
(setq inhibit-startup-screen t)

;; visual options
(load-theme 'rebecca t)
(set-face-attribute 'default nil
  :family "Monaco" :height 230 :weight 'normal)

;; best vim emulation mode ever
(require 'evil)
(evil-mode 1)

;; sets fish as the command for ansi-term
(setq explicit-shell-file-name "/usr/local/bin/fish")

;; ~/.emacs.d/init.el is a symlink
(setq vc-follow-symlinks t)

;; enable C-x C-f fuzzy finding
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; move with shift + directional arrows
(windmove-default-keybindings)

;; hook prettier to run in the major web modes
(require 'prettier-js)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

;; disable backups
(setq make-backup-files nil)

;; don't move back the cursor one space after exiting evil edit mode
(setq evil-move-cursor-back nil)

;; configure indentation
(setq-default tab-width 2)
(setq js-indent-level 2)

;; yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; default to UTF-8
(prefer-coding-system 'utf-8)

(use-package minions
  :ensure t
  :config (minions-mode 1))

(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package swiper
	:ensure t
	:config
	(progn
		(ivy-mode 1)
		(setq ivy-use-virtual-buffers t)
		(setq enable-recursive-minibuffers t)
		(global-set-key "\C-s" 'swiper)
		(global-set-key (kbd "C-c C-r") 'ivy-resume)
		(global-set-key (kbd "<f6>") 'ivy-resume)
		(global-set-key (kbd "M-x") 'counsel-M-x)
		(global-set-key (kbd "C-x C-f") 'counsel-find-file)
		(global-set-key (kbd "<f1> f") 'counsel-describe-function)
		(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
		(global-set-key (kbd "<f1> l") 'counsel-find-library)
		(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
		(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
		(global-set-key (kbd "C-c g") 'counsel-git)
		(global-set-key (kbd "C-c j") 'counsel-git-grep)
		(global-set-key (kbd "C-c k") 'counsel-ag)
		(global-set-key (kbd "C-x l") 'counsel-locate)
		(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
		(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
	))

(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-:") 'ivy-dired)
(define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur)
