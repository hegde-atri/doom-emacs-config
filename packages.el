(package! centered-cursor-mode)

(package! beacon)

(package! tao-theme)

(unpin! doom-modeline)

(package! org-appear
  :recipe (:host github
           :repo "awth13/org-appear"))

(package! org-auto-tangle)

(package! ob-ledger :recipe (:local-repo "lisp/ob-ledger"))

(package! ob-mermaid)

(unpin! org-roam)
(package! org-roam-ui)

(package! simplenote2)

(after! lsp-mode
  (setq lsp-inlay-hint-enable t
        lsp-auto-guess-root nil))

(package! evil-nerd-commenter)

(package! prettier)

(package! svelte-mode)

(package! prisma-mode :recipe (:host github :repo "pimeys/emacs-prisma-mode" :branch "main"))

(package! yuck-mode)

(package! ellama)

;; (package! copilot
;;   :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
