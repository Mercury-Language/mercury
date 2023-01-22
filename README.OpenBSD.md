Mercury on OpenBSD
==================

Tested on OpenBSD 6.2 amd64 with `clang` 4.0 and `gcc` 4.9.

The base version of `gcc` is 4.2.1, but you can install a more recent
version from ports with

```
    $ pkg_add gcc
```

Then direct Mercury to use `egcc` by running:

```
    $ CC=egcc ./configure <your normal configure arguments>
```
