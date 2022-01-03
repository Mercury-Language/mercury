Mercury
=======

[Mercury](http://www.mercurylang.org/) is a logic/functional programming
language which combines the clarity and the expressiveness of declarative
programming with advanced static analysis and error detection features.

More information is available on the
[website's about pages](http://www.mercurylang.org/about.html),
in other README files in the source code repository, and in the
[documentation](http://www.mercurylang.org/documentation/documentation.html).

Small sample programs written in Mercury can be found in the [samples](samples)
directory of the source code repository.

## README files

The Mercury compiler has a number of different
[backends](http://www.mercurylang.org/about/backends.html)
and works on different operating systems.
Specific information is contained in individual README files:

  * [Bootstrapping](README.bootstrap) discusses how to get Mercury installed.

    This is important as the Mercury compiler is written in Mercury.

  * C Low-level backend

    This backend works well with GCC but also works with:

      * [Clang](README.clang)

  * High-level backend targets

      * C
      * [C#](README.CSharp)
      * [Java](README.Java)

  * Platforms

      * [Docker](README.Docker)
      * [Linux](README.Linux)
        ([PPC](README.Linux-PPC),
        [m68k](README.Linux-m68k))
      * [MacOS X](README.MacOS)
      * [FreeBSD](README.FreeBSD)
      * [OpenBSD](README.OpenBSD)
      * [AIX](README.AIX)
      * [HP-UX](README.HPUX)
      * [Solaris](README.Solaris)
      * [Windows](README.MS-Windows)
        ([Visual C](README.MS-VisualC),
        [MinGW](README.MinGW),
        [Cygwin](README.Cygwin))
      * [Cross compilation](README.cross)
      * [x86](README.x86)

## Other information

See the current [release notes](RELEASE_NOTES) for the latest stable release.
The [history](HISTORY) file is relevant if you want to find out more about the
past development of Mercury.
[News](NEWS) lists any current or future enhancements (but this isn't
always up-to-date).
The [limitations](LIMITATIONS.md) file lists a number of ways in which the
Mercury implementation does not yet meet its goals.

## Information for developers

If you are considering contributing to the Mercury project the website
contains some documents that may be helpful.  These include a document about
[contributions in general](http://www.mercurylang.org/development/contributions.html) and
[specific information](http://www.mercurylang.org/development/developer.html)
about contributing such as coding styles.

## Contact

See [our contact page](http://www.mercurylang.org/contact.html).
