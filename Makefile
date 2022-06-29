.PHONY: build
build:
	stack build

.PHONY: build-tests
build-tests:
	stack build --test --no-run-tests

.PHONY: ghci
ghci:
	stack ghci

.PHONY: test
test:
	stack test

# Intended for use in local dev
.PHONY: test-postgresql
test-postgresql: reset-pgsql
	stack test esqueleto:postgresql

.PHONY: test-mysql
test-mysql:
	stack test esqueleto:mysql

.PHONY: test-ghci
test-ghci:
	stack ghci esqueleto:test:sqlite

.PHONY: test-ghcid
test-ghcid:
	ghcid -c "stack ghci --ghci-options -fobject-code esqueleto --test" \
		--warnings \
		--restart "stack.yaml" \
		--restart "esqueleto.cabal" \
		--test main

.PHONY: test-ghcid-build
test-ghcid-build:
	ghcid -c "stack ghci --ghci-options -fobject-code esqueleto --test" \
		--warnings \
		--restart "stack.yaml" \
		--restart "esqueleto.cabal"

.PHONY: haddock doc
haddock: doc
doc:
	$(STACK) haddock

.PHONY: init-pgsql
init-pgsql:
	sudo -u postgres -- createuser -s esqutest

.PHONY: reset-pgsql
reset-pgsql:
	-sudo -u postgres dropdb esqutest
	-sudo -u postgres dropuser esqutest
	echo "CREATE USER esqutest WITH PASSWORD 'esqutest';" | sudo -u postgres psql
	sudo -u postgres createdb -O esqutest esqutest
