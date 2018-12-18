#!/bin/env bash

STACK_YAML=stack.yaml

stack build --fast expected-compile-failures:exe:write-with-read-role && exit 1
