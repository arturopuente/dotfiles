;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Port of ~/dev/arturo/dotfiles/.emacs.d/init.el onto Doom.
;;
;; Anything the old config did that Doom already does by default has been
;; dropped rather than restated -- UTF-8 coding systems, y/n prompts,
;; delete-selection-mode, startup screen suppression, trailing-whitespace
;; trimming, backup/lockfile handling, winner-mode, show-paren, menu/tool bar,
;; gc tuning, macOS dired/trash/frame tweaks, and the helpful C-h rebinds.
;; What remains below is the part that is actually yours.


;;
;;; Identity

(setq user-full-name "Arturo Puente"
      user-mail-address "arturopuentevc@gmail.com")


;;
;;; UI

;; The old config used Monaco 190 (1/10 pt) on macOS, Ubuntu Mono 150 elsewhere.
(setq doom-font (if (featurep :system 'macos)
                    (font-spec :family "Monaco" :size 19)
                  (font-spec :family "Ubuntu Mono" :size 15)))

(setq doom-theme 'modus-operandi)

(after! modus-themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-to-toggle '(modus-operandi modus-vivendi)
        ;; modus-themes 4.x replaced `modus-themes-region' with palette
        ;; overrides; this is the equivalent of the old '(bg-only no-extend).
        modus-themes-common-palette-overrides
        '((bg-region bg-lavender)
          (fg-region unspecified))))

(map! "<f5>" #'modus-themes-toggle)

(setq display-line-numbers-type 'relative)

;; Start maximized.
(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(blink-cursor-mode 0)
(setq column-number-mode t)

;; Collapse minor modes into a single modeline menu.
(use-package! minions
  :config (minions-mode 1))

(after! neotree
  (setq neo-theme 'ascii))

;; Highlight colours for evil-multiedit / iedit regions.
(custom-set-faces!
  '(iedit-occurrence :background "#F50" :foreground "#000")
  '(flycheck-error   :background "red"  :foreground "white"))


;;
;;; Editor behaviour

(setq default-directory "~/dev")

(setq-default word-wrap t
              fill-column 80)

;; One space ends a sentence, not two.
(setq sentence-end-double-space nil)

(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; ESC acts as C-g everywhere.
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(map! "<M-backspace>" #'my-backward-delete-word)

;; Keeps evil's yank/change/delete out of the system clipboard; only explicit
;; simpleclip commands touch it.
;; NOTE: this is the setting most likely to feel odd alongside Doom's own
;; clipboard handling. Comment out both lines to fall back to Doom's default.
(use-package! simpleclip
  :config
  (simpleclip-mode 1)
  (setq select-enable-clipboard nil))


;;
;;; Indentation

(setq-default indent-tabs-mode nil
              tab-width 2
              evil-shift-width 2)

(setq-default js-indent-level 2
              typescript-indent-level 2
              python-indent-offset 4)

;; :editor whitespace +guess gives us dtrt-indent, which infers tab-width /
;; evil-shift-width / the mode's own offset var from the file's contents. Doom
;; disables it inside projects by default, on the theory that projects declare
;; intent via .editorconfig or .dir-locals.el -- but most repos don't, so turn
;; it on. The setq-default values above stay as the fallback when detection is
;; inconclusive. Excluded by design: emacs-lisp, org (see
;; `+whitespace-guess-excluded-modes').
(setq +whitespace-guess-in-projects t)

(after! web-mode
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-css-colorization t))

;; NOTE: the old config had (global-set-key (kbd "TAB") #'tab-to-tab-stop).
;; Left off deliberately -- under Doom, TAB drives corfu completion, snippet
;; field navigation and org cycling, all of which a global rebind breaks.
;; Uncomment the next line if you want it back anyway.
;; (map! "TAB" #'tab-to-tab-stop)

(map! "<S-tab>" #'evil-shift-left
      "s-]"     #'evil-shift-right-line
      "s-["     #'evil-shift-left-line)


;;
;;; Evil

(setq evil-move-cursor-back nil)

(map! :n "C-f" #'evil-scroll-up)

;; Doom's :editor multiple-cursors already binds evil-multiedit to M-d/M-D;
;; these restore the old s-d/s-D on top of that. In visual state we match the
;; selection, elsewhere the symbol at point.
(after! evil-multiedit
  (evil-define-key* 'visual 'global
    (kbd "s-d") #'evil-multiedit-match-and-next
    (kbd "s-D") #'evil-multiedit-match-and-prev)
  (evil-define-key* '(normal insert) 'global
    (kbd "s-d") #'evil-multiedit-match-symbol-and-next
    (kbd "s-D") #'evil-multiedit-match-symbol-and-prev)
  ;; RET already toggles the region under point by default.
  (evil-define-key* '(normal insert) evil-multiedit-mode-map
    (kbd "s-j") #'evil-multiedit-next
    (kbd "s-k") #'evil-multiedit-prev))


;;
;;; Windows

(defun split-right-and-switch ()
  "Split the window vertically and switch to that window."
  (interactive)
  (split-window-right)
  (other-window 1 nil))

(defun split-below-and-switch ()
  "Split the window horizontally and switch to that window."
  (interactive)
  (split-window-below)
  (other-window 1 nil))

(after! ace-window
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (ace-window-display-mode))


;;
;;; Leader bindings
;;
;; Doom owns SPC, and its own prefixes are left intact -- so the old
;; SPC g / SPC n / SPC o / SPC c are now SPC g g (magit-status), SPC o - (dired),
;; SPC o p (neotree), and gs SPC (avy). The keys below were free in Doom.
;; Note SPC 1-9 live under SPC TAB for workspaces, so the digits are ours.

(map! :leader
      :desc "Delete other windows"  "1" #'delete-other-windows
      :desc "Split right & switch"  "2" #'split-right-and-switch
      :desc "Split below & switch"  "3" #'split-below-and-switch
      :desc "Winner undo"           "9" #'winner-undo
      :desc "Winner redo"           "0" #'winner-redo
      :desc "Delete window"         "-" #'delete-window
      :desc "VC annotate"           "v" #'vc-annotate
      (:prefix ("j" . "javascript")
       :desc "Jest popup"           "t" #'jest-popup))


;;
;;; Super-key layer
;;
;; This is carried over wholesale -- it was the part of the old config with the
;; most muscle memory attached. ivy/counsel equivalents map onto consult.

(map! "s-f" #'consult-line              ; was swiper
      "s-F" #'deadgrep
      "s-b" #'consult-buffer            ; was ivy-switch-buffer
      "s-p" #'projectile-find-file
      "s-o" #'ace-window
      "M-s-o" #'ace-swap-window
      "s-t" #'+neotree/open
      "s-g" #'minibuffer-keyboard-quit
      "s-9" #'winner-undo
      "s-0" #'winner-redo
      "s-{" #'previous-buffer
      "s-}" #'next-buffer
      "C-=" #'er/expand-region
      "C-c o" #'occur)

;; projectile is lazy-loaded, so its keymap only exists once it is on.
(after! projectile
  (map! "s-P" projectile-command-map))

;; s-j/s-k moved candidate selection in ivy; vertico is the equivalent here.
(after! vertico
  (map! :map vertico-map
        "s-j" #'vertico-next
        "s-k" #'vertico-previous
        "C-o" #'embark-export           ; was ivy-occur
        "<return>" #'vertico-directory-enter))


;;
;;; occur

(after! replace
  (evil-set-initial-state 'occur-mode 'normal)
  (map! :map occur-mode-map
        "C-x C-q" #'occur-edit-mode
        "C-c C-c" #'occur-mode-goto-occurrence
        :map occur-edit-mode-map
        "C-x C-q" #'occur-cease-edit
        "C-c C-c" #'occur-cease-edit))


;;
;;; Git

(after! magit
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))


;;
;;; Languages

;; The old config ran prettier-js only in JS/TS buffers. Doom's :editor format
;; module uses apheleia (which drives prettier itself); enabling it per-mode
;; rather than globally keeps that same narrow scope.
(add-hook! (js-mode js-ts-mode typescript-mode typescript-ts-mode
            tsx-ts-mode web-mode)
  #'apheleia-mode)

(after! markdown-mode
  (setq markdown-command "multimarkdown"))


;;
;;; Org

(setq org-directory "~/org/")
(setq initial-major-mode 'org-mode)

;; NOTE: `evil-auto-indent' is a plain defcustom -- evil does *not* make it
;; buffer-local -- so a bare `setq' here would clobber the global value and kill
;; o/O indentation in every buffer (and `initial-major-mode' being org means
;; *scratch* triggers it at startup). `setq-local' is required.
(add-hook 'org-mode-hook (lambda () (setq-local evil-auto-indent nil)))

(after! org
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-window-setup 'current-window
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t))

;; NOTE: the old config called `org-babel-do-load-languages' with an explicit
;; list (ruby, calc, python, lisp, shell, restclient, js). Doom advises that
;; function to `ignore' (lang/org/config.el) because it lazy-loads the matching
;; ob-* library the first time you execute a block of that language. So the list
;; is unnecessary here -- all seven still work, they just load on demand.
;; ob-restclient is pulled in via packages.el so the restclient blocks resolve.
