%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug412.
:- interface.

:- import_module bool.

% Avoid a warning about the module not exporting anything.
:- type blah
    --->    blah.

:- implementation.

% Test a warning about a module imported in the interface not being used
% in the interface.
:- type blah2
    --->    blah2(bool).

:- import_module bitmap.
:- import_module bit_buffer.
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
