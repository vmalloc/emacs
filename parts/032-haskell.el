(use-package haskell-mode
  :ensure t
  :mode "\\.hs$"
  :config
  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
  (setq haskell-process-suggest-hoogle-imports nil)

  (add-hook 'haskell-mode-hook
            (lambda ()
              (turn-on-haskell-indentation)
              (define-key haskell-mode-map (kbd "<f9>") 'haskell-process-cabal-build)
              ))

  (add-hook 'haskell-interactive-mode-hook
            (lambda ()
              (define-key haskell-interactive-mode-map (kbd "<f9>") 'haskell-process-cabal-build)
              )))
