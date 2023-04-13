;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Atri Hegde"
      user-mail-address "atri@hegdeatri.com")

(setq doom-font (font-spec :family "JetBrains Mono" :size 15 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 15 :weight 'regular))

(setq doom-theme 'doom-moonlight)

(setq doom-modeline-enable-word-count t)
(setq doom-modeline-height 30     ;; sets modeline height
      doom-modeline-bar-width 5   ;; sets right bar width
      ;; doom-modeline-persp-name t  ;; adds perspective name to modeline
      ;; doom-modeline-persp-icon t  ;; adds folder icon next to persp name
      doom-modeline-icon t
      doom-modeline-major-mode-color-icon t
      doom-modeline-battery t
      doom-modeline-major-mode-color-icon t
      doom-modeline-enable-word-count nil) ;; word count

;; (setq doom-themes-treemacs-theme "doom-colors")
;; (customize-set-variable 'doom-themes-treemacs-theme "doom-colors")

(setq display-line-numbers-type 'relative)

(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

(setq fancy-splash-image "~/.config/doom/doom-emacs-dash.png")
(add-hook! '+doom-dashboard-functions :append
           (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Any text editor can save your files, only Emacs can save your soul!")))

(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(setq shell-file-name "/bin/bash")
(setq-default shell-file-name "/bin/bash")
(setenv "SHELL" shell-file-name)

(map! :leader
      (:prefix ("b" . "buffer")
       :desc "switch buffer"          "b" #'(lambda () (interactive) (counsel-switch-buffer))))

(map! :leader
      (:prefix ("=" . "open config")
       :desc "Hyprland"      "h" #'(lambda () (interactive) (find-file "~/.config/hypr/hypr.org"))
       :desc "zshrc"         "z" #'(lambda () (interactive) (find-file "~/.zshrc"))
       :desc "eww"           "e" #'(lambda () (interactive) (find-file "~/.config/eww/eww.org"))
       :desc "nushell"       "n" #'(lambda () (interactive) (find-file "~/.config/nushell/nushell.org"))
       :desc "foot"          "f" #'(lambda () (interactive) (find-file "~/.config/foot/foot.org"))))

(map! :leader
      (:prefix ("p" . "open config")
       (:prefix ("m" . "make tasks")
       :desc "run-last"       "r" #'(lambda () (interactive) (+make/run-last))
       :desc "run"          "R" #'(lambda () (interactive) (+make/run)))))

(setq org-directory "~/org/")

(defun ha/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
;; Setting up hook for visual fill
(add-hook 'org-mode 'ha/org-mode-visual-fill)

(defun ha/org-setup ()
  (setq org-log-done 'time)
  (setq org-hide-emphasis-markers t)
  ;; Enlarge latex preview
  (plist-put org-format-latex-options :scale 2)
  (add-hook! org-mode :append #'org-appear-mode)
)


(defun ha/org-font-setup ()
  ;; Doesn't work in Doom emacs
  ;;(font-lock-add-keywords 'org-mode
  ;;                       '(("^ *\\([-]\\) "
  ;;                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Change font size of headings.
  (dolist (face '((org-level-1 . 1.5)
                  (org-level-2 . 1.4)
                  (org-level-3 . 1.3)
                  (org-level-4 . 1.25)
                  (org-level-5 . 1.2)
                  (org-level-6 . 1.15)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.05)))
    (set-face-attribute (car face) nil :font "Overpass" :weight 'medium :height (cdr face)))

;; Fonts in org
  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(after! org
  (ha/org-setup)
  (ha/org-font-setup)
  (setq
        org-ellipsis " ▼ "
        org-hide-emphasis-markers t
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")))

(after! org
  (setq org-roam-directory "~/org/roam")
  (setq org-roam-capture-templates
    '(("d" "default" plain
       "%?"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+startup: latexpreview\n")
       :unnarrowed t)
      ("m" "module" plain
       ;; (file "<path to template>")
       "\n* Module details\n\n- %^{Module code}\n- Semester: %^{Semester}\n\n* %?"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+startup: latexpreview\n")
       :unnarrowed t)
      ("b" "book notes" plain
       "\n* Source\n\n- Author: %^{Author}\n- Title: ${title}\n- Year: %^{Year}\n\n%?"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+startup: latexpreview\n")
       :unnarrowed t)
    )
  )
  (setq org-roam-dailies-capture-templates
    '(("d" "default" entry "* %<%H:%M>: %?"
       :ifnew (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))
    )
  )
  (org-roam-setup))

(setq
   ;; org-fancy-priorities-list '("❗" "⚠" "👆")
   org-fancy-priorities-list '("🟥" "🟧" "🟨")
   ;;org-priority-faces
   ;;'((?A :foreground "#ff6c6b" :weight bold)
   ;;  (?B :foreground "#98be65" :weight bold)
   ;;  (?C :foreground "#c678dd" :weight bold))
   org-agenda-block-separator 8411)

(setq org-agenda-custom-commands
      '(("v" "A better agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (tags "PRIORITY=\"B\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Medium-priority unfinished tasks:")))
          (tags "PRIORITY=\"C\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Low-priority unfinished tasks:")))
          (tags "customtag"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tasks marked with customtag:")))

          (agenda "")
          (alltodo "")))))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t)
)

(after! ob-mermaid
  :config
  (setq ob-mermaid-cli-path "/usr/bin/mmdc"))

(org-babel-do-load-languages
    'org-babel-load-languages
    '((mermaid . t)
      (scheme . t)))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;    :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; (use-package! tree-sitter
;;  :config
;;  (require 'tree-sitter-langs)
;;  (global-tree-sitter-mode)
;;  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package! lsp
    :custom
    (lsp-rust-analyzer-server-display-inlay-hints t)
)

(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '((".*\\.astro\\'" . astro-mode))
              auto-mode-alist))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(astro-mode . "astro"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("astro-ls" "--stdio"))
                    :activation-fn (lsp-activate-on "astro")
                    :server-id 'astro-ls)))

(add-hook! astro-mode #'lsp-deferred)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; if you use typescript-mode
(add-hook 'typescript-mode-hook #'setup-tide-mode)
;; if you use treesitter based typescript-ts-mode (emacs 29+)
;; (add-hook 'typescript-ts-mode-hook #'setup-tide-mode)
;; For tsx files.
(add-hook 'tsx-ts-mode-hook #'setup-tide-mode)
