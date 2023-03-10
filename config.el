;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Atri Hegde"
      user-mail-address "atri@hegdeatri.com")

(setq doom-font (font-spec :family "JetBrains Mono" :size 15 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 15 :weight 'regular))

(setq doom-theme 'doom-palenight)

(setq doom-modeline-enable-word-count t)

(setq display-line-numbers-type 'relative)

(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

(setq fancy-splash-image "~/.config/doom/doom-emacs-dash.png")
(add-hook! '+doom-dashboard-functions :append
           (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Any text editor can save your files, only Emacs can save your soul!")))

(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

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
  (plist-put org-format-latex-options :scale 1.6)
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

(after! org-auto-tangle
  (add-hook! org-mode :append #'org-auto-tangle-mode)
)

(after! ob-mermaid
  :config
  (setq ob-mermaid-cli-path "/usr/bin/mmdc"))

(org-babel-do-load-languages
    'org-babel-load-languages
    '((mermaid . t)
      (scheme . t)))
