#!/bin/sh -e
# This script should be run in a fresh git checkout in order to initialise
# the necessary git submodules and produce the configure script.

if [ ! -e boehm_gc/libatomic_ops ]; then
    echo "Setting up submodules"
    if git submodule --quiet init; then
        git submodule update --remote --checkout
        ln -s ../libatomic_ops boehm_gc/libatomic_ops || \
            cp -R libatomic_ops boehm_gc/libatomic_ops
    else
        echo "There was a problem configuring the submodules.  If the"
        echo "repositories could not be found then edit .git/config and run"
        echo "get submodule update"
        exit 1
    fi
fi

echo "Setting up autotools"
aclocal -I m4
autoconf
