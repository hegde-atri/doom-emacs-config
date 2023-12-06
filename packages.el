(unpin! org-roam)
(package! org-roam-ui)

(package! svg-lib)
(package! svg-tag-mode)

(package! notebook-mode :recipe (:local-repo "lisp/notebook"))

(package! org-auto-tangle)

(package! ob-mermaid)

(package! mermaid-mode)

(package! evil-tutor)

(unpin! lsp-mode)

(package! svelte-mode)

(package! prisma-mode :recipe (:host github :repo "pimeys/emacs-prisma-mode" :branch "main"))

(package! prettier)

(package! ellama)

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
