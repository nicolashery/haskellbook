all: setup build test lint

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install hlint stylish-haskell

.PHONY: build
build:
	stack build --pedantic --test --no-run-tests

.PHONY: test
test:
	stack test

.PHONY: lint
lint:
	hlint .

.PHONY: ghci
ghci:
	stack ghci

.PHONY: ghci-test
ghci-test:
	stack ghci haskellbook:spec

.PHONY: run
run:
	stack exec haskellbook
