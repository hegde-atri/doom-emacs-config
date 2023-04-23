(package! org-appear
  :recipe (:host github
           :repo "awth13/org-appear"))

(package! org-auto-tangle)

(package! ob-ledger :recipe (:local-repo "lisp/ob-ledger"))

(package! ob-mermaid)

(unpin! org-roam)
(package! org-roam-ui)

;; (package! tree-sitter)
;; (package! tree-sitter-langs)

(package! tramp)

(unpin! lsp-mode)

(package! prettier)

(package! tide)
