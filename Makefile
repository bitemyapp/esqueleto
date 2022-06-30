STACK := stack --jobs $(shell nproc)

match ?=

STACK_TEST_ARGS := $(if $(match),--test-arguments "--match $(match)",)

.PHONY: build
build:
	$(STACK) build

.PHONY: build-tests
build-tests:
	$(STACK) build --test --no-run-tests

.PHONY: ghci
ghci:
	$(STACK) ghci

.PHONY: test
test:
	$(STACK) test $(STACK_TEST_ARGS)

# Intended for use in local dev
.PHONY: test-postgresql
test-postgresql: reset-pgsql
	$(STACK) test esqueleto:postgresql $(STACK_TEST_ARGS)

.PHONY: test-mysql
test-mysql:
	$(STACK) test esqueleto:mysql $(STACK_TEST_ARGS)

.PHONY: test-ghci
test-ghci:
	$(STACK) ghci esqueleto:test:sqlite $(STACK_TEST_ARGS)

.PHONY: test-ghcid
test-ghcid:
	ghcid -c "$(STACK) ghci --ghci-options -fobject-code esqueleto --test" \
		--warnings \
		--restart "stack.yaml" \
		--restart "esqueleto.cabal" \
		--test main

.PHONY: test-ghcid-build
test-ghcid-build:
	ghcid -c "$(STACK) ghci --ghci-options -fobject-code esqueleto --test" \
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
