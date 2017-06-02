doc/index.html: Maker.lhs
	awk -f lhs2html Maker.lhs > $@

nix: default.nix
	nix-shell -A mkrfuzz.env mkrfuzz.nix --command 'cabal configure'
ghci:
	cabal repl --ghc-options="-fobject-code -O2 -Wall -fno-warn-name-shadowing -fno-warn-missing-signatures"

default.nix: mkrfuzz.cabal; cabal2nix . > default.nix;

dai:; cabal build dai

docker:
	docker build -t makerdao/faker .
	docker run -it --rm makerdao/faker

test:
	cabal build
	./dist/build/mkrfuzz/mkrfuzz > solidity/src/fuzz.sol
	make -C solidity test
