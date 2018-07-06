%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%-----------------------------------------------------------------------------%
%
% This simple program reads in terms from standard input, and outputs
% those terms as indented structures, annotating each occurrence of
% a function symbol that is an operator with the operator information
% (infix/prefix/postfix, precedence and associativity) that is applicable
% to that occurrence. It stops at end-of-file, and also on any error.
%
% This can be useful when one wants to see how Mercury parses a term
% containing operators, and (which is sometimes just as important)
% *why* it parses a term the way it does.
%
% One can also explore the effects of different operator tables on
% term parsing by simply replacing the operator table assigned to Ops in main
% with a table of one's own choosing.
%
%-----------------------------------------------------------------------------%

:- module show_ops.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module ops.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module varset.

main(!IO) :-
    read_term(ReadTerm, !IO),
    (
        ReadTerm = eof
    ;
        ReadTerm = error(_, _),
        io.write_string("error\n", !IO)
    ;
        ReadTerm = term(VarSet, Term),
        io.nl(!IO),
        Ops = ops.init_mercury_op_table,
        print_term(0, Ops, VarSet, Term, !IO),
        main(!IO)
    ).

:- pred print_term(int::in, ops.mercury_op_table::in,
    varset::in, term::in, io::di, io::uo) is det.

print_term(Indent, Ops, VarSet, Term, !IO) :-
    indent(Indent, !IO),
    (
        Term = variable(Var, _Context),
        varset.lookup_name(VarSet, Var, VarName),
        io.write_string(VarName, !IO),
        io.nl(!IO)
    ;
        Term = functor(Functor, ArgTerms, _Context),
        (
            Functor = atom(Name),
            io.write_string(Name, !IO),
            ( if ops.lookup_op_infos(Ops, Name, HeadInfo, TailInfos) then
                print_applicable_ops([HeadInfo | TailInfos], ArgTerms, !IO)
            else
                true
            )
        ;
            Functor = integer(_Base, Integer, _Signedeness, _Size),
            % XXX should not just write this out as a decimal integer.
            io.write_string(to_string(Integer), !IO)
        ;
            Functor = string(Str),
            io.write_string("\"", !IO),
            io.write_string(Str, !IO),
            io.write_string("\"", !IO)
        ;
            Functor = float(Float),
            io.write_float(Float, !IO)
        ;
            Functor = implementation_defined(Str),
            io.write_string("impl_def ", !IO),
            io.write_string(Str, !IO)
        ),
        io.nl(!IO),
        list.foldl(print_term(Indent + 1, Ops, VarSet), ArgTerms, !IO)
    ).

:- pred print_applicable_ops(list(op_info)::in, list(term)::in,
    io::di, io::uo) is det.

print_applicable_ops([], _ArgTerms, !IO).
print_applicable_ops([HeadInfo | TailInfos], ArgTerms, !IO) :-
    HeadInfo = op_info(Class, Prio),
    (
        Class = infix(LAssoc, RAssoc),
        ( if ArgTerms = [_, _] then
            io.format(" infix %d %sf%s",
                [i(Prio), s(assoc_str(LAssoc)), s(assoc_str(RAssoc))], !IO)
        else
            true
        )
    ;
        Class = binary_prefix(LAssoc, RAssoc),
        ( if ArgTerms = [_, _] then
            io.format(" binary_prefix %d %sf%s",
                [i(Prio), s(assoc_str(LAssoc)), s(assoc_str(RAssoc))], !IO)
        else
            true
        )
    ;
        Class = prefix(Assoc),
        ( if ArgTerms = [_] then
            io.format(" prefix %d f%s",
                [i(Prio), s(assoc_str(Assoc))], !IO)
        else
            true
        )
    ;
        Class = postfix(Assoc),
        ( if ArgTerms = [_] then
            io.format(" postfix %d %sf",
                [i(Prio), s(assoc_str(Assoc))], !IO)
        else
            true
        )
    ),
    print_applicable_ops(TailInfos, ArgTerms, !IO).

:- func assoc_str(ops.assoc) = string.

assoc_str(x) = "x".
assoc_str(y) = "y".

:- pred indent(int::in, io::di, io::uo) is det.

indent(N, !IO) :-
    ( if N > 0 then
        io.write_string("  ", !IO),
        indent(N - 1, !IO)
    else
        true
    ).
