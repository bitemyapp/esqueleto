build: build-7.10

build-7.10:
	STACK_YAML="stack-7.10.yaml" stack build

build-8.0:
	STACK_YAML="stack-8.0.yaml" stack build

ghci:
	stack ghci

test:
	stack test

test-mysql:
	stack test --flag esqueleto:mysql

test-pgsql:
	stack test --flag esqueleto:postgresql

test-ghci:
	stack ghci esqueleto:test:test

test-mysql-ghci:
	stack ghci esqueleto:test:test --flag esqueleto:mysql

.PHONY: build build-7.10 build-8.0 ghci test test-mysql test-pgsql test-ghci
