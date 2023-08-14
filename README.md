Mercury
=======

[Mercury](https://www.mercurylang.org/) is a logic/functional programming
language which combines the clarity and the expressiveness of declarative
programming with advanced static analysis and error detection features.

More information is available on the
[website's about pages](https://www.mercurylang.org/about.html),
in other README files in the source code repository, and in the
[documentation](https://www.mercurylang.org/documentation/documentation.html).

Small sample programs written in Mercury can be found
in the [samples](samples) and [extras](extras) directories
of the source code repository.

## README files

The Mercury compiler has two different
[backends](https://www.mercurylang.org/about/backends.html)
and works on different operating systems.
Specific information is contained in individual README files:

  * [Bootstrapping](README.bootstrap) discusses how to get Mercury installed.

    This is important, as the Mercury compiler is written in Mercury.

  * C Low-level backend

    This backend works well with GCC but also works with:

      * [Clang](README.clang.md)

  * High-level backend targets

      * C
      * [C#](README.CSharp.md)
      * [Java](README.Java.md)

  * Supported operating systems

      * [Linux](README.Linux)
        ([AArch64](README.Linux-aarch64.md),
        [PPC](README.Linux-PPC),
        [m68k](README.Linux-m68k))
      * [macOS](README.macOS.md)
      * [FreeBSD](README.FreeBSD.md)
      * [OpenBSD](README.OpenBSD.md)
      * [AIX](README.AIX.md)
      * [HP-UX](README.HPUX.md)
      * [Solaris](README.Solaris.md)
      * [Windows](README.MS-Windows.md)
        ([Visual C](README.MS-VisualC.md),
        [MinGW](README.MinGW),
        [Cygwin](README.Cygwin.md)

  * Other platform information
      * [Cross compilation](README.cross.md)
      * [Docker](README.Docker)
      * [x86](README.x86)

## Other information

See the current [release notes](RELEASE_NOTES) for the latest stable release.
The [news](NEWS.md) file lists any recent changes.
The [history](HISTORY) file is relevant
if you want to find out more about the past development of Mercury.
The [limitations](LIMITATIONS.md) file lists some ways
in which the Mercury implementation does not yet meet its goals.

## Information for developers

If you are considering contributing to the Mercury project,
the website contains some documents that may be helpful.
These include a document about
[contributions in general](https://www.mercurylang.org/development/contributions.html) and
[specific information](https://www.mercurylang.org/development/developer.html)
about contributing such as coding styles.

## Contact

See [our contact page](https://www.mercurylang.org/contact.html).
