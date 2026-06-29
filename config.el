;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Atri Hegde"
      user-mail-address "atri@example.com")

(after! super-save
  :init
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration 2)
  (setq super-save-silent t)
  ;; But not for remote files
  (setq super-save-remote-files nil)
  :config
  (super-save-mode +1))

;; super save doesn't work without this...
(after! org
  (super-save-mode +1))

(map! :leader
      :desc "Save Buffer" "SPC" #'save-buffer)

(setq-default tab-width 2)

(setq doom-theme 'doom-palenight)

;; JetBrains Mono
;; (setq doom-font (font-spec :family "JetBrains Mono" :size 15 :weight 'medium)
;;       doom-variable-pitch-font (font-spec :family "JetBrains Mono" :sie 15 :weight 'medium))
;; Terminess Mono
(setq doom-font (font-spec :family "Terminess Nerd Font Mono" :size 17 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "Terminess Nerd Font Mono" :size 17 :weight 'medium))

(when (display-graphic-p)
  (doom-big-font-mode t))

;; Default global scroll margin
(setq-default scroll-margin 7)

;; Apart from these modes which should have a scroll margin of 0
(dolist (mode '(Info-mode term-mode eshell-mode shell-mode erc-mode vterm-mode comint-mode))
  (add-hook (intern (format "%s-hook" mode))
            (lambda () (setq-local scroll-margin 0))))

;; Use relative line numbers
(setq display-line-numbers-type 'relative)

(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(after! doom-modeline
  (display-battery-mode t)
  ;; Also have the file icon
  (setq doom-modeline-major-mode-icon t))

(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `with-eval-after-load' block, otherwise Doom's defaults may override your
;; settings. E.g.
;;
;;   (with-eval-after-load 'PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look them up).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
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

(setq org-modern-table-vertical 1)
(setq org-modern-table t)

(setq writeroom-width 75)
(setq writeroom-mode-line t)

(use-package! websocket
  :after org-roam)
(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t)
  (setq org-roam-ui-follow t)
  (setq org-roam-ui-update-on-save t)
  (setq org-roam-ui-open-on-start t))

;; (after! corfu
;;   (setq corfu-preselect 'first))
;; (setq +corfu-want-ret-to-confirm t)
(after! corfu
     (setq corfu-preselect 'first)
     (map! :map corfu-map
           :gi "TAB" #'corfu-insert
           :gi [tab] #'corfu-insert))

(after! lsp-rust
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"))

(add-load-path! "vendor/crates.el")
(require 'crates)

(add-to-list 'auto-mode-alist '("/Cargo\\.toml\\'" . conf-toml-mode))

(defun +doom-enable-crates-mode-maybe ()
  (when (and buffer-file-name
             (string= (file-name-nondirectory buffer-file-name) "Cargo.toml"))
    (crates-mode 1)))

(add-hook 'find-file-hook #'+doom-enable-crates-mode-maybe)
(add-hook 'conf-toml-mode-hook #'+doom-enable-crates-mode-maybe)
(add-hook 'toml-ts-mode-hook #'+doom-enable-crates-mode-maybe)
(add-hook 'toml-mode-hook #'+doom-enable-crates-mode-maybe)

(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (+doom-enable-crates-mode-maybe)))

(with-eval-after-load 'python
  (set-eglot-client! '(python-mode python-ts-mode) '("ty" "server")))
(with-eval-after-load 'python
  (set-formatter! 'ruff :modes '(python-mode python-ts-mode)))
