package = esqueleto
stack = STACK_YAML='stack.yaml' stack

build:
	$(stack) build

build-tests:
	$(stack) build --test --no-run-tests

ghci:
	$(stack) ghci

ghcid:
	ghcid -c "$(stack) ghci $(package):lib --test --ghci-options='-fobject-code -fno-warn-unused-do-bind' --main-is $(package):test:read-write"

test:
	$(stack) test

# Intended for use in local dev
test-postgresql: reset-pgsql
	$(stack) test $(package):postgresql

test-mysql:
	$(stack) test $(package):mysql

test-readwrite:
	$(stack) test $(package):test:read-write

test-readwrite-ghci:
	$(stack) ghci $(package):test:read-write

# sudo -u postgres createuser -s - esqueleto-test
reset-pgsql:
	-sudo -u postgres dropdb esqutest
	-sudo -u postgres dropuser esqutest
	echo "CREATE USER esqutest WITH PASSWORD 'esqutest';" | sudo -u postgres psql
	sudo -u postgres createdb -O esqutest esqutest

.PHONY: build build-7.10 build-8.0 ghci test test-ghci
