(require 'cl-lib)
(require 'package)

(let ((melpa '("melpa" . "https://melpa.org/packages/"))
      (org-mode '("org" . "https://orgmode.org/elpa/")))
  (add-to-list 'package-archives melpa t)
  (add-to-list 'package-archives org-mode t)
)

;; misc
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(load-theme 'rebecca t)
(set-face-attribute 'default nil
  :family "Monaco" :height 230 :weight 'normal)

(require 'evil)
(evil-mode 1)

(setq explicit-shell-file-name "/usr/local/bin/fish")

(tool-bar-mode -1) 
(global-linum-mode t)
(setq inhibit-startup-screen t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
