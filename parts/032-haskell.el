(autoload 'haskell-mode (in-modes-d "haskell-mode/haskell-mode.el") nil t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(autoload 'turn-on-haskell-ghci "haskell-ghci"
  "Turn on interaction with a GHCi interpreter." t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(add-to-list 'load-path (in-modes-d "ghc-mod/elisp"))
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))
(setq ghc-module-command "~/.cabal/bin/ghc-mod")

