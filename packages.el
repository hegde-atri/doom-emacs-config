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

(unpin! lsp-mode)



(package! prettier)

(package! tide)

;; (package! codeium :recipe (:host github :repo "Exafunction/codeium.el"))

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
