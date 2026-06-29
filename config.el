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

;; Save files quicker/easier
(map! :leader
      :desc "Save Buffer" "SPC" #'save-buffer)

;; Tab size
(setq-default tab-width 2)

(setq doom-theme 'doom-palenight)

;; JetBrains Mono
(setq doom-font (font-spec :family "JetBrains Mono" :size 17 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :sie 17 :weight 'medium))
;; Terminess Mono
;; (setq doom-font (font-spec :family "Terminess Nerd Font Mono" :size 17 :weight 'medium)
;;       doom-variable-pitch-font (font-spec :family "Terminess Nerd Font Mono" :size 17 :weight 'medium))

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
