pr:
	hub pull-request -b ardumont:master

install:
	uname -a
	sudo apt-get install -y haskell-platform
	cabal --version
	cabal update &&	cabal install cabal-install

deps:
	ghc --version
	cabal --version
	cabal install base process mtl QuickCheck HUnit parsec

sandbox-init:
	cabal sandbox init

sandbox-delete:
	cabal sandbox delete

cabal-init:
	cabal init

run:
	cabal run

test:
	cabal test --show-details=always

build:
	cabal configure --enable-tests
	cabal build

cabal2nix:
	cabal2nix --sha256 dummy-sha1-for-the-moment write-yourself-a-scheme-lab.cabal > write-yourself-a-scheme-lab.nix

run-nix-shell:
	nix-shell --pure write-yourself-a-scheme-lab.nix

ghc:
	ghc --make -fglasgow-exts -o Main Main.hs
