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

To build the final product deployable (needs to be done on linux atm):
```
nix-build nix/backend/backend.nix
```

To deploy, you need to ONCE do:
```
nixops create nix/service-configuration.nix nix/ec2.nix -d apertivo
nixops deploy -d apertivo
```

Not exactly a long term solution, but since I use ngrok to get https, on the host you then need to:
```
nix-env -iA nixos.wget
wget https://bin.equinox.io/c/4VmDzA7iaHb/ngrok-stable-linux-amd64.zip
nix-env -iA nixos.unzip
unzip ngrok-stable-linux-amd64.zip
./ngrok authtoken <auth token from your ngrok accountr>
nohup ./ngrok http 3000 &
```
then you can go to the website and see your https URL.

On backend update, you just need to run:
```
nixops deploy -d apertivo
```