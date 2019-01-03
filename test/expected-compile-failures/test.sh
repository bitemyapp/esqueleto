#!/bin/env bash

# This script attempts to build each executable in the package, which should all
# fail with a compiler error. If any executable builds successfully, then we exit
# the script.

# We have to use 2>&1 because `stack ide targets` outputs to stderr for some
# reason.
for target in $(stack ide targets 2>&1 | grep exe); do
    echo "Building target: $target"
    if stack build --fast $target; then
        exit 1
    fi
done
