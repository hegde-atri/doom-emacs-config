(add-to-list 'load-path "~/.config/doom/scripts/")

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Atri Hegde"
      user-mail-address "me@hegdeatri.com")

(setq display-line-numbers-type 'relative)

(setq scroll-margin 7)

(defun my-set-scroll-margin ()
  "Set scroll-margin based on the current major mode."
  (setq-local scroll-margin
              (cond ((derived-mode-p 'Info-mode) 0)
                    ((derived-mode-p 'term-mode) 0)
                    ((derived-mode-p 'eshell-mode) 0)
                    ((derived-mode-p 'shell-mode) 0)
                    ((derived-mode-p 'erc-mode) 0)
                    ((derived-mode-p 'vterm-mode) 0)
                    (t 7))))

(add-hook 'after-change-major-mode-hook #'my-set-scroll-margin)

;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face

(setq doom-font (font-spec :family "JetBrainsMono" :size 15 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 12)
      doom-big-font (font-spec :family "JetBrainsMono" :size 24))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic))
  ;; '(font-lock-keyword-face :slant italic))

(setq doom-theme 'doom-palenight)

(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

(defun ha/toggle-window-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha-transparency 90))
    (pcase (frame-parameter nil 'alpha-background)
      (alpha-transparency (set-frame-parameter nil 'alpha-background 100))
      (t (set-frame-parameter nil 'alpha-background alpha-transparency)))))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 35)
  (doom-modeline-bar-width 1)
  (display-battery-mode t)
  (doom-modeline-time t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-persp-name nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil)
  (setq display-time-mode t))

(setq fancy-splash-image
      (concat doom-private-dir "splash/" "vagabond.png"))

(map! :leader
      (:prefix ("=" . "open config")
       :desc "Hyprland"      "h" #'(lambda () (interactive) (find-file "~/.config/hypr/hypr.org"))
       :desc "zshrc"         "z" #'(lambda () (interactive) (find-file "~/.zshrc"))
       :desc "eww"           "e" #'(lambda () (interactive) (find-file "~/.config/eww/eww.org"))
       :desc "nushell"       "n" #'(lambda () (interactive) (find-file "~/.config/nushell/nushell.org"))
       :desc "foot"          "f" #'(lambda () (interactive) (find-file "~/.config/foot/foot.org"))))

(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(require 'zone)
(require 'zone-words)
(eval-after-load "zone"
  '(unless (memq 'zone-words (append zone-programs nil))
     (setq zone-programs (vconcat [zone-words]))))

(zone-when-idle 600)

(setq org-directory "~/org/")

(after! org
  (setq org-log-done 'time)
  (setq org-hide-emphasis-markers t)
  (setq org-startup-with-inline-images t)
  (setq
   org-ellipsis " ▼ "
   org-hide-emphasis-markers t
   ;; org-superstar-headline-bullets-list '("⁙" "⁘" "⁖" "❋" "✸" "✹")
   ;; org-superstar-headline-bullets-list '("⁖" "○" "◉" "●" "✸" "✿")
   org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
   )

  ;; after org continues

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                ))
  (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))
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
(set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(d)" "DONE")
        (sequence "IDEA(i)" "SCRIPTED(s)" "RECORDED(r)" "EDITED")
        (sequence "CLIENT(c)" "SCRIPTED(s)" "SENT")))

(setq org-todo-keyword-faces '(
                               ("IDEA" . (:foreground "#ffcc00" :bold t :weight bold)) ; yellow
                               ("SCRIPTED" . (:foreground "#b8e4f9" :bold t :weight bold)) ; light blue
                               ("RECORDED" . (:foreground "#ff84c9" :bold t :weight bold)) ; pink
                               ("CLIENT" . (:foreground "#ffcc00" :bold t :weight bold)) ; yellow
                               ("EDITED" . ( :foreground "gray65" :bold t :weight bold)) ; grey
                               ("SENT" . ( :foreground "gray65" :bold t :weight bold)) ; grey
                               ))

(setq org-pretty-entities t)

(plist-put org-format-latex-options :scale 0.5)
(setq org-highlight-latex-and-related '(latex))
(plist-put org-format-latex-options :background "Transparent")

(add-hook 'org-mode-hook 'org-fragtog-mode)

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
;; (org-roam-db-autosync-enable)

;; writeroom mode bydefault for org roam buffers.
(add-hook 'org-mode-hook #'+zen/toggle t)
;; Keep fonts in writeroom mode.
(add-hook 'org-mode-hook #'buffer-face-mode)
;; Enable svg-tag-mode
(add-hook 'org-mode-hook #'svg-tag-mode)
;; after org ends
)

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package! svg-lib
  :init (add-hook 'after-setting-font-hook (lambda () (setq svg-lib-style-default (svg-lib-style-compute-default)))))

(load "/home/mizuuu/.config/doom/scripts/svg-tags.el")

(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("org-plain-latex"
               "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

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
      (schema . t)))

;; (after! mermaid-mode)

(setq shell-file-name "/bin/zsh")
(setq-default shell-file-name "/bin/zsh")
(setenv "SHELL" shell-file-name)

(map! :leader "w c" nil)

(map! :leader
      (:prefix ("b" . "buffer")
       :desc "Format buffer" "f" #'lsp-format-buffer))

(after! lsp-mode
  (setq lsp-inlay-hint-enable t
        lsp-inlay-hints-mode t))

(setq treesit-language-source-alist
      '((astro "https://github.com/virchau13/tree-sitter-astro")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))

(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '((".*\\.astro\\'" . astro-mode))
              auto-mode-alist))

(defun astro-get-tsserver ()
    ""
    (f-join (lsp-workspace-root) "node_modules/typescript/lib/tsserverlibrary.js"))


(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(astro-ts-mode . "astro"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("astro-ls" "--stdio"))
                  :activation-fn (lsp-activate-on "astro")
                  :server-id 'astro-ls)))

(after! poetry
  (remove-hook 'python-mode-hook #'poetry-tracking-mode)
  (add-hook 'python-mode-hook 'poetry-track-virtualenv))

(after! lsp
  (add-to-list 'lsp-language-id-configuration '(".*\\.html\\.erb$" . "html"))
)

(use-package! ellama
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
          ;; (llm-ollama-host "10.27.27.100")
          (make-llm-ollama
           :chat-model "zephyr" :embedding-model "zephyr")))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))
