%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Running "mmc --make-interface" on this module used to result in a
% compiler abort. This was because
%
% - due to the abstract-exported type depending on bool, the compiler
%   deciced that the import of the bool module in the implementation section
%   was needed, but
%
% - the module bool was imported in the interface as well.
%
% The abort was caused by an overzealous sanity check that insisted
% on the absence of any such duplication.
%

:- module int_imp_test_2.
:- interface.

:- import_module bool.

:- type t1
    --->    f1(bool).

:- type t2.

:- implementation.

:- type t2 == bool.
