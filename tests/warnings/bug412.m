%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug412.
:- interface.

% Avoid a warning about the module not exporting anything.
:- type blah
    --->    blah.

:- implementation.

:- import_module bitmap.
:- import_module bit_buffer.
:- import_module bool.
:- import_module builtin.
:- import_module char.
:- import_module diet.
:- import_module digraph.
:- import_module enum.
:- import_module io.
:- import_module ops.
:- import_module pprint.
:- import_module rtree.
:- import_module store.
:- import_module stream.
:- import_module string.
:- import_module string.builder.
:- import_module table_statistics.
:- import_module term.
:- import_module univ.
