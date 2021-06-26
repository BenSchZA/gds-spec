ghc_version = 8.10.4

init:
	nix-shell --packages ghc cabal-install --run "cabal init"

update:
	cabal update

install-ghc:
	ghcup install ghc $(ghc_version)

set-ghc:
	ghcup set ghc $(ghc_version)

run:
	cabal run an-informal-spec
