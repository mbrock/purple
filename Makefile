all: doc/MakerDao900.pdf
Fuzz.gen.feature: Mkrfuzz.hs MakerDao900.lhs
	docker build -t makerdao/mkrfuzz .
	docker run -it --rm makerdao/mkrfuzz >$@

test: default.nix MakerDao900.lhs
	nix-shell -A mkrfuzz.env mkrfuzz.nix \
	  --command 'runghc MakerDao900.lhs'

doc/MakerDao900.pdf: MakerDao900.lhs
	docker build -t makerdao/lhs2tex doc
	docker run -it --rm -v `pwd`:/src:ro -v `pwd`/doc:/doc makerdao/lhs2tex

nix-ghci: default.nix
	nix-shell -A mkrfuzz.env mkrfuzz.nix --command 'cabal repl'
nix-run: default.nix
	nix-shell -A mkrfuzz.env mkrfuzz.nix --command 'cabal run'

default.nix: mkrfuzz.cabal; cabal2nix . > default.nix
