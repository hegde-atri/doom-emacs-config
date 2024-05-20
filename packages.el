(package! transpose-frame)

(package! org-fragtog)

(unpin! org-roam)
(package! org-roam-ui)

(package! svg-lib)
(package! svg-tag-mode)

(package! notebook-mode :recipe (:local-repo "lisp/notebook"))

(package! org-auto-tangle)

(package! ob-mermaid)

(package! mermaid-mode)

(package! evil-tutor)

(package! evil-escape :disable t)

(unpin! lsp-mode)

(package! astro-ts-mode :recipe (:host github :repo "Sorixelle/astro-ts-mode" :branch "master"))

(package! svelte-mode)

(package! prisma-mode :recipe (:host github :repo "pimeys/emacs-prisma-mode" :branch "main"))

(package! prettier)

(package! ellama)

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
