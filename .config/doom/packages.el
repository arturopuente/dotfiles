;;; $DOOMDIR/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Only packages from the old ~/.emacs.d/init.el that Doom does NOT already
;; ship. Everything else comes from an enabled module -- see init.el.
;;
;; Already provided by Doom, deliberately NOT repeated here:
;;   evil, evil-collection, evil-surround, evil-multiedit, avy, ace-window,
;;   expand-region, drag-stuff, undo-tree, helpful, which-key, yasnippet,
;;   company/corfu, magit, git-link, diff-hl (vc-gutter), projectile, neotree,
;;   flycheck (checkers/syntax), web-mode, json-mode, yaml-mode, markdown-mode,
;;   edit-indirect, restclient, inf-ruby, robe, chruby, python-pytest,
;;   exec-path-from-shell (os/macos).

;; Theme. Not bundled with this Emacs build, so pull it explicitly.
(package! modus-themes)

;; Keeps evil's yank/change/delete off the system clipboard.
(package! simpleclip)

;; The opposite of `fill-region' (M-q).
(package! unfill)

;; Minor-mode menu that stays out of the way in the modeline.
(package! minions)

;; ripgrep UI, bound to s-F.
(package! deadgrep)

;; Editable grep buffers (pairs with embark-export under vertico).
(package! wgrep)

;; Open the current file/line on GitHub.
(package! github-browse-file)

;; JS test runner, bound under SPC j.
(package! jest)

;; Ruby: tail and colourise development.log.
(package! rails-log-mode)

;; org-babel restclient blocks.
(package! ob-restclient)

;; NOTE: dropped on purpose, and why:
;;   tide, rjsx-mode, typescript-mode -> superseded by (javascript +lsp)
;;   ivy, counsel, swiper, flx        -> superseded by vertico + consult + orderless
;;   company                          -> superseded by corfu
;;   prettier-js                      -> superseded by apheleia (:editor format)
;;   xterm-color                      -> magit handles ANSI natively now
