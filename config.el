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
(setq-default tab-width 4)

(setq doom-theme 'doom-dracula)

;; JetBrains Mono
;; (setq doom-font (font-spec :family "JetBrains Mono" :size 17 :weight 'medium)
;;       doom-variable-pitch-font (font-spec :family "JetBrains Mono" :sie 17 :weight 'medium))
;; Terminess Mono
(setq doom-font (font-spec :family "Terminess Nerd Font Mono" :size 17 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "Terminess Nerd Font Mono" :size 17 :weight 'medium))

;; (when (display-graphic-p)
;;   (doom-big-font-mode t))

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

(setq org-agenda-files '("~/org/roam/agenda/"))

(setq org-log-done 'time)
(setq org-hide-emphasis-markers t)
(setq org-startup-with-inline-images t)
(setq org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆"))

(after! org
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local doom-modeline-enable-word-count t))))

(after! org
  (add-hook 'org-mode-hook
            (lambda ()
              (face-remap-add-relative 'default :family "Overpass")))
  (set-face-attribute 'org-block nil :family "Terminess Nerd Font Mono")
  (set-face-attribute 'org-block-begin-line nil :family "Terminess Nerd Font Mono")
  (set-face-attribute 'org-block-end-line nil :family "Terminess Nerd Font Mono")
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(after! org
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.0)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil
                        :font "Iosevka Aile"
                        :weight 'medium
                        :height (cdr face)))
  (set-face-attribute 'org-document-title nil
                      :font "Iosevka Aile"
                      :weight 'bold
                      :height 1.3))

(after! org
  (add-hook 'org-mode-hook
            #'org-fragtog-mode))

(after! org
  (plist-put org-format-latex-options :scale 0.85)
  (setq org-highlight-latex-and-related '(latex))
  (plist-put org-format-latex-options :background "Transparent"))

(after! org
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

  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("report"
                   "\\documentclass{report}"
                   ("\\chapter{%s}" . "\\section*{%s}")
                   ("\\section{%s}" . "\\subsection*{%s}")
                   ("\\subsection{%s}" . "\\subsubsection*{%s}")
                   ("\\subsubsection{%s}" . "\\paragraph*{%s}")
                   ("\\paragraph{%s}" . "\\subparagraph*{%s}"))))

  ;; Use PDF Tools for AUCTeX previews and keep rendered PDFs current.
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" "TeX-pdf-tools-sync-view"))
        TeX-source-correlate-start-server t)
  (add-hook 'pdf-view-mode-hook #'auto-revert-mode)

  ;; Use minted for source blocks. Requires pygments and shell escape support.
  (setq org-latex-src-block-backend 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory build %f"))

  (require 'ox-latex))

(after! org-roam
  (setq org-roam-directory (file-truename "~/org/roam/")))

(after! org-roam
  (setq org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%H:%M>: %?"
           :ifnew (file+head "%<%Y-%m-%d>.org"
                             "#+title: %<%Y-%m-%d>\n")))))

(setq org-modern-table-vertical 1)
(setq org-modern-table t)

(setq writeroom-width 75)
(setq writeroom-mode-line t)

(use-package! mermaid-mode
  :mode "\\.mmd\\'"
  :config
  (setq mermaid-mmdc-location (or (executable-find "mmdc") "mmdc")
        mermaid-output-format ".svg"))

(use-package! ob-mermaid
  :after org
  :config
  (setq ob-mermaid-cli-path (or (executable-find "mmdc") "mmdc"))
  (setq org-babel-default-header-args:mermaid
        '((:results . "file graphics")
          (:exports . "results")))
  (add-to-list 'org-src-lang-modes '("mermaid" . mermaid))
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '((mermaid . t)))))

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
