;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Arturo Puente"
      user-mail-address "arturopuentevc@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

(setq doom-font (font-spec :family "Monaco" :size 19)
       doom-variable-pitch-font (font-spec :family "Monaco" :size 19))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-operandi)

(after! hl-line
  (set-face-attribute 'region nil :background "#bdfcc9")
  (set-face-background 'hl-line "#fffdd0")
)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(after! ivy-prescient (
  ivy-prescient-mode 1))

(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (defalias 'yes-or-no-p 'y-or-n-p)
;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(simpleclip-mode 1)
(setq select-enable-clipboard nil)

;; evil indentation
(global-set-key (kbd "s-]") 'evil-shift-right-line)
(global-set-key (kbd "s-[") 'evil-shift-left-line)

(setq evil-move-cursor-back nil)

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

(map! :leader
      :desc "Keep one window" "1" #'delete-other-windows
      :desc "Split vertical" "2" #'split-right-and-switch
      :desc "Split horizontal" "3" #'split-below-and-switch
      :desc "Undo window split" "9" #'winner-undo
      :desc "Redo window split" "0" #'winner-redo
      :desc "Delete window" "-" #'delete-window
      :desc "Magit" "g" #'magit-status
)

(global-set-key (kbd "s-o") 'ace-window)
(global-set-key (kbd "M-s-o") 'ace-swap-window)
(global-set-key (kbd "s-O") 'ns-open-file-using-panel)
(global-set-key (kbd "s-p") '+ivy/projectile-find-file)
(global-set-key (kbd "s-t") 'neotree-toggle)
(global-set-key (kbd "s-b") 'consult-recent-file)
(global-set-key (kbd "s-f") 'swiper)
(global-set-key (kbd "s-F") '+default/search-project)

(bind-key "s-{" 'previous-buffer)
(bind-key "s-}" 'next-buffer)

;; enable horizontal scrolling
(setq mouse-wheel-flip-direction t)
(setq mouse-wheel-tilt-scroll t)

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)

(if (display-graphic-p)
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
)

(remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)
(setq undo-tree-auto-save-history nil)

(defun my-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (company-indent-or-complete-common nil)))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map company-active-map
         ("<tab>" . 'my-tab)
         ("TAB" . 'my-tab)
         :map company-mode-map
         ("<tab>" . 'my-tab)
         ("TAB" . 'my-tab)))