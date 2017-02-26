doc/maker.pdf: Maker.lhs
	docker build -t makerdao/lhs2tex doc
	docker run -it --rm -v `pwd`:/src:ro -v `pwd`/doc:/doc makerdao/lhs2tex

nix-ghci: default.nix
	nix-shell -A mkrfuzz.env mkrfuzz.nix --command 'cabal repl --ghc-options="-fobject-code -O2 -Wall -fno-warn-name-shadowing"'
nix-run: default.nix
	nix-shell -A mkrfuzz.env mkrfuzz.nix --command 'cabal run'
nix-sim: default.nix
	nix-shell -A mkrfuzz.env mkrfuzz.nix --command 'cabal run mkrsim'
nix-plot: default.nix
	nix-shell -A mkrfuzz.env mkrfuzz.nix --command 'cabal run mkrplot'
        
default.nix: mkrfuzz.cabal; cabal2nix . > default.nix

docker:
	docker build -t makerdao/faker .
	docker run -it --rm makerdao/faker
        