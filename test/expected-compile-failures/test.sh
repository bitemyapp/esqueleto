#!/bin/env bash

if stack build --fast expected-compile-failures:exe:write-with-read-role; then
    exit 1
else
    exit 0
fi
