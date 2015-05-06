This code contains Mercury FFI bindings to the multi-precision integer library
libtommath. It works with the C grades that use the Boehm garbage collector.

The binding relies on the fact that at compile time, one can redefine the
malloc/free functions called from libtommath. These are defined as calls to the
Boehm-GC, so that allocated mp_int values and the contained data-structures get
garbage collected in the end. The libtommath authors ask to call the mp_clear
function as finalizer when freeing an allocated mp_int value, this is not done
in this current binding.

To use the binding, do the following:

1) download libtommath sources:

$ git clone https://github.com/libtom/libtommath.git

tested on 036d697caa206224f3766c05d3e736cbca36f577

2) make libtommath.a with redefined MALLOC/FREE macros:

$ cd libtommath
$ env CFLAGS='-DXMALLOC=MR_GC_malloc -DXREALLOC=MR_GC_realloc -DXCALLOC=MR_GC_calloc -DXFREE=GC_free' make

3) copy mp_int.m, the compiled libtommath.a, tommath.h, tommath_class.h and
   tommath_superclass.h into your project folder

4) build your project using mmc with the additional option
   '--link-object libtommath.a'

Some more discussion about this can be found in the following discussion threads
of the Mercury-users mailing-list:

http://www.mercurylang.org/list-archives/users/2015-April/007878.html
http://www.mercurylang.org/list-archives/users/2015-May/007919.html
