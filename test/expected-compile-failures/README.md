# expected-compile-failures

This subdirectory contains a stack project for expected compilation failures. To
add a new "test case", create a new `executable` stanza in the `package.yaml`
file. The Travis CI test script ([`test.sh`](test.sh)) will attempt to compile
the executable and will exit with an error if it successfully compiled.
