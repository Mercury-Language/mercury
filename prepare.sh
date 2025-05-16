#!/bin/sh
# This script should be run in a fresh git checkout in order to initialise
# the necessary git submodules and produce the configure script.
set -e

if test ! -e boehm_gc/.git; then
    echo "Setting up submodules"
    if git submodule --quiet init; then
        git submodule update --init --recursive
    else
        echo "There was a problem configuring the boehm_gc submodule."
        echo "If its repository could not be found, then"
        echo "first edit .gitmodules to specify its current location,"
        echo "and then run \"git submodule update --recursive\"."
        exit 1
    fi
fi

echo "Setting up autotools"
aclocal -I m4
autoconf -Wno-obsolete
