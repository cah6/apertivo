Repo for Apertivo: a happy hour finder app 
---

See https://cah6.github.io/technology/nix-haskell-3/ for frontend workflow commands.

## Manual setup

1. Get `es_password.secret` for the backend (local dev talks to remote elasticsearch atm)
2. Need `~/.ssh/my-free-tier.pem` to deploy backend

## Backend workflow

That post doesn't have any backend info, will go over that here.

Enter the shell:
```
nix-shell nix/backend/shell.nix
```

In that shell, you can do workflow things with cabal new-style commands:
```
cabal new-run backend
```