build:
	stack build

build-tests:
	stack build --test --no-run-tests

ghci:
	stack ghci

test:
	stack test

# Intended for use in local dev
test-postgresql: reset-pgsql
	stack test esqueleto:postgresql

test-mysql:
	stack test esqueleto:mysql

test-ghci:
	stack ghci esqueleto:test:sqlite

test-ghcid:
	ghcid -c "stack ghci --ghci-options -fobject-code esqueleto:test:sqlite"

# sudo -u postgres createuser -s - esqueleto-test
reset-pgsql:
	-sudo -u postgres dropdb esqutest
	-sudo -u postgres dropuser esqutest
	echo "CREATE USER esqutest WITH PASSWORD 'esqutest';" | sudo -u postgres psql
	sudo -u postgres createdb -O esqutest esqutest

.PHONY: build build-7.10 build-8.0 ghci test test-ghci
