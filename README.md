Mercury
=======

[Mercury](http://www.mercurylang.org/) is a logic/functional programming
language which combines the clarity and the expressiveness of declarative
programming with advanced static analysis and error detection features.

This README file contains very little information itself.  It is intended to
direct the reader to the specific information they're looking for.
More detailed information is available on the [website's about
pages](http://www.mercurylang.org/about.html), in other README files in the
root directory of the source code repository, and in the
[documentation](http://www.mercurylang.org/documentation/documentation.html).

## README files

Mercury has a number of different
[Backends](http://www.mercurylang.org/about/backends.html) These include two
different C backends, plus backends for Java, C# and Erlang.
Mercury also works on different operating systems and with different C
compilers.  Specific information is contained in individual README files as
follows:

 * [Bootstrapping](README.bootstrap) discusses how to get Mercury installed.

   This is important as Mercury is written in Mercury, so installing Mercury
   without an existing installation of Mercury requires bootstrapping.

 * C Low-level backend

   This backend works well with GCC but also works with:

  * [Clang](README.clang)
  * [lcc](README.lcc)

 * High-level backend targets
  * C
  * [C#](README.CSharp)
  * [Erlang](README.Erlang)
  * [Java](README.Java)
 * Experimental / Obsolete backends
  * [.Net IL](README.DotNet)
 * Unix
  * [Linux](README.Linux)
   * [Alpha](README.Linux-Alpha)
   * [PPC](README.Linux-PPC)
   * [m68k](README.Linux-m68k)
  * [MacOS](README.MacOS)
  * [FreeBSD](README.FreeBSD)
  * [AIX](README.AIX)
  * [HP-UX](README.HPUX)
  * [IRIX-5](README.IRIX-5)
  * [Solaris](README.Solaris)
 * [Windows](README.MS-Windows)
  * Using the [Visual C compiler](README.MS-VisualC)
  * Using the [MinGW gcc compiler](README.MinGW)
  * Using the [Cygwin](README.Cygwin) environment
 * Cross compilation
  * [MinGW](README.MinGW-cross)

## Other information

Other information is available in text files in the source code repository.

See the current [release notes](RELEASE_NOTES) for the latest stable release.
The [history](HISTORY) file is relevant if you want to find out more about the
past development of Mercury.
[News](NEWS) lists any current or future enhancements (but this isn't
always up-to-date).
The [limitations](LIMITATIONS) file lists a number of ways in which the
Mercury implementation does not yet meet its goals.

## Information for developers

If you are considering contributing to the Mercury project the website
contains that may be helpful.  These include a document about
[contributions in general](http://www.mercurylang.org/development/contributions.html) and
[specific information](http://www.mercurylang.org/development/developer.html)
about contributing such as coding styles.

## Contact

Finally contact information is provided on [our contact
page](http://www.mercurylang.org/contact.html).

