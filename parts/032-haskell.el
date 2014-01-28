(require 'haskell-mode)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(autoload 'turn-on-haskell-ghci "haskell-ghci"
  "Turn on interaction with a GHCi interpreter." t)

(setq haskell-process-suggest-hoogle-imports nil)

(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-indentation)
            (define-key haskell-mode-map (kbd "<f9>") 'haskell-process-cabal-build)
            ))

(add-hook 'haskell-interactive-mode-hook
          (lambda ()
            (define-key haskell-interactive-mode-map (kbd "<f9>") 'haskell-process-cabal-build)
            ))
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
;; (add-to-list 'load-path (in-modes-d "ghc-mod/elisp"))
;; (autoload 'ghc-init "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))
;; (setq ghc-module-command "~/.cabal/bin/ghc-mod")

