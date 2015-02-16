%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The missing constraint at the spot marked XXX causes the following
% assertion failure in rotd-2007-08-19 and before:
%
% Software Error: map.lookup: key not found
%   Key Type: term.var(parse_tree.prog_data.prog_var_type)
%   Key Value: var(4)
%   Value Type: parse_tree.prog_data.mer_type

:- module typeclass_test_13.
:- interface.

:- import_module list.
:- import_module string.

:- typeclass output(U) where [
    pred add_quoted_string(string::in, U::di, U::uo) is det,
    pred add_list(list(string)::in,
        pred(string, U, U)::in(pred(in, di, uo) is det), U::di, U::uo) is det
].

:- pred mercury_format_pragma_foreign_export_enum_overrides(
    list(string)::in, U::di, U::uo) is det <= output(U).

:- implementation.

mercury_format_pragma_foreign_export_enum_overrides(Overrides, !U) :-
    add_list(Overrides, mercury_format_pragma_foreign_export_enum_override,
        !U).

:- pred mercury_format_pragma_foreign_export_enum_override(
    string::in, U::di, U::uo) is det. % XXX <= output(U).

mercury_format_pragma_foreign_export_enum_override(CtorName, !U) :-
    mercury_format_bracketed_sym_name(CtorName, !U),
    add_quoted_string(CtorName, !U).

:- pred mercury_format_bracketed_sym_name(string::in,
    U::di, U::uo) is det <= output(U).

mercury_format_bracketed_sym_name(_, !U).
