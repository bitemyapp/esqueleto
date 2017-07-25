build: build-7.10

build-7.10:
	STACK_YAML="stack-7.10.yaml" stack build

build-8.0:
	STACK_YAML="stack-8.0.yaml" stack build

ghci:
	stack ghci

ghcid:
	ghcid -c "stack ghci esqueleto:lib --test"


test:
	stack test

# Intended for use in local dev
test-postgresql: reset-pgsql
	stack test --flag esqueleto:postgresql

test-mysql: reset-mysql
	stack test --flag esqueleto:mysql

test-ghci:
	stack ghci esqueleto:test:test

test-mysql-ghci:
	stack ghci esqueleto:test:test --flag esqueleto:mysql

# sudo -u postgres createuser -s - esqueleto-test
reset-pgsql:
	-sudo -u postgres dropdb esqutest
	-sudo -u postgres dropuser esqutest
	echo "CREATE USER esqutest WITH PASSWORD 'esqutest';" | sudo -u postgres psql
	sudo -u postgres createdb -O esqutest esqutest

reset-mysql:
	-echo "DROP DATABASE esqutest;" | sudo -u mysql mysql -u root
	-echo "DROP USER 'esqutest'@'localhost';" | sudo -u mysql mysql -u root
	echo "CREATE USER 'esqutest'@'localhost' IDENTIFIED BY 'esqutest';" | sudo -u mysql mysql -u root
	echo "CREATE DATABASE esqutest;" | sudo -u mysql mysql -u root
	echo "FLUSH PRIVILEGES;" | sudo -u mysql mysql -u root
	echo "grant all privileges on esqutest.* to 'esqutest'@'localhost';" | sudo -u mysql mysql -u root

.PHONY: build build-7.10 build-8.0 ghci test test-ghci test-mysql test-postgresql reset-pgsql reset-mysql
