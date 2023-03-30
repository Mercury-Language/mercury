%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006, 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: genotype.m.
% Main author: samrith.
%
%---------------------------------------------------------------------------%

:- module genotype.
:- interface.

:- import_module tausworthe3.

:- import_module io.
:- import_module list.

:- type genotype.

:- type flag.

    % read_genotypes(Path, Genotypes, !IO):
    %
    % Reads in a list of genotypes from the given file.  A genotype is
    % made up of a set of flags.  Each genotype within a file is
    % separated by a newline character, and each flag within a genotype
    % is separated by one or more spaces.
    %
:- pred read_genotypes(string::in, list(genotype)::out, io::di, io::uo) is det.

    % crossover(Mother, Father, Son, Daughter, !RNG):
    %
    % This predicate takes two parent genotypes and randomly selects
    % optimisation flags from each to create two new child genotypes.
    % Note that we don't actually care whether a genotype is male or
    % female; the terms mother and father, and son and daughter are just
    % used to differentiate the two different parents and children.
    %
    % A "cut and splice" algorithm was chosen here as it is simple
    % to implement and seems to be well suited to the data structure
    % used.  Many common crossover methods assume the genotype is a
    % fixed-length bit-array.
    %
:- pred crossover(genotype::in, genotype::in, genotype::out, genotype::out,
    RNG::in, RNG::out) is det <= random(RNG, Seed).

    % mutation(Flags, Child, Mutant, !RNG).
    %
    % This predicate randomly mutates a genotype.
    %
    % This predicate is implemented by choosing a compiler flag at
    % random and toggling that flag in the child genotype.
    %
:- pred mutation(list(flag)::in, genotype::in, genotype::out,
    RNG::in, RNG::out) is det <= random(RNG, Seed).

    % print_genotypes(Path, Genotypes, !IO):
    %
    % Prints out a list of genotypes to the given file, in the same
    % format as expected by read_genotypes/4.
    %
:- pred print_genotypes(string::in, list(genotype)::in, io::di, io::uo) is det.

    % genotype_to_string(Genotype) = String.
    %
    % Returns a string representation of the genotype.
    %
:- func genotype_to_string(genotype) = string is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.

%---------------------------------------------------------------------------%

:- type genotype == set(flag).

:- type flag == string.

%---------------------------------------------------------------------------%
%
% Reading in a list of genotypes.
%
% This section contains the implementation of the read_genotypes/4
% predicate, which parses a file containing the flags to be passed to the
% compiler.
%
% The parser is implemented using Mercury's DCG notation.  The many/4
% predicate is a higher-order DCG-rule that takes another DCG-rule as its
% first argument.  It is used to parse a list of things such as a list of
% genotypes or a list of flags.
%

read_genotypes(Path, Genotypes, !IO) :-
    io.open_input(Path, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        io.read_file(Stream, ReadResult, !IO),
        (
            ReadResult = ok(File),
            ( if many(genotype, Genotypes0, File, []) then
                Genotypes = Genotypes0,
                io.close_input(Stream, !IO)
            else
                require.error("parse error while reading genotypes")
            )
        ;
            ReadResult = error(_, ErrorCode),
            io.error_message(ErrorCode, ErrorMessage),
            require.error(ErrorMessage)
        )
    ;
        OpenResult = error(ErrorCode),
        io.error_message(ErrorCode, ErrorMessage),
        require.error(ErrorMessage)
    ).

:- pred many(pred(T, list(char), list(char))::(pred(out, in, out) is semidet),
    list(T)::out, list(char)::in, list(char)::out) is semidet.

many(P, Ps) -->
    ( if P(X) then 
        many(P, Xs),
        { Ps = [X | Xs] }
    else
        { Ps = [] }
    ).

:- pred genotype(genotype::out, list(char)::in, list(char)::out) is semidet.

genotype(Genotype) -->
    many(pred(' '::out, in, out) is semidet --> [' '], _DiscardLeadingSpaces),
    many(flag, Flags),
    ['\n'],
    { set.list_to_set(list.map(string.strip, Flags), Genotype) }.

:- pred flag(flag::out, list(char)::in, list(char)::out) is semidet.

flag(Flag) -->
    double_dash(DoubleDash),
    many(other, Others),
    { Flag = string.from_char_list(DoubleDash ++ Others) }.

:- pred double_dash(list(char)::out, list(char)::in, list(char)::out)
    is semidet.

double_dash(DoubleDash) -->
    ['-', '-'],
    { DoubleDash = ['-', '-'] }.

:- pred other(char::out, list(char)::in, list(char)::out) is semidet.

other(Other) -->
    \+ ['-', '-'],
    \+ ['\n'],
    [Other].

%---------------------------------------------------------------------------%
%
% Genetic operators that operate on the genotype.
%
% This section contains the predicates for the genetic operators that
% require access to (and knowledge of) the representation of the genotype.
% These include the crossover (or recombination) and mutation operators.
%

crossover(Mother, Father, Son, Daughter, !RNG) :-
    list.map2_foldl(cut, [Mother, Father], PartsOfSon, PartsOfDaughter, !RNG),
    Son = set.union_list(PartsOfSon),
    Daughter = set.union_list(PartsOfDaughter).

:- pred cut(genotype::in, genotype::out, genotype::out, RNG::in,
        RNG::out) is det <= random(RNG, Seed).

cut(Parent, PartOfSon, PartOfDaughter, !RNG) :-
    ( if
        set.count(Parent, NumFlags),
        NumFlags \= 0
    then
        next(NextRandomInt, !RNG),
        CrossoverPoint = NextRandomInt mod NumFlags
    else
        CrossoverPoint = 0
    ),
    set.to_sorted_list(Parent, List),
    list.det_split_list(CrossoverPoint, List, Start, End),
    set.list_to_set(Start, PartOfSon),
    set.list_to_set(End, PartOfDaughter).

mutation(Flags, !Genotype, !RNG) :-

    list.length(Flags, NumFlags),
    next(Next, !RNG),
    Index = Next mod NumFlags,
    list.det_index0(Flags, Index, Flag),

    ( if set.member(Flag, !.Genotype) then
        set.delete(Flag, !Genotype)
    else
        set.insert(Flag, !Genotype)
    ).

%---------------------------------------------------------------------------%
%
% Printing out a list of genotypes.
%
% This section contains the implementation of the print_genotypes/4
% predicate, which creates a file containing the flags to be passed to the
% compiler.
%

print_genotypes(Path, Genotypes, !IO) :-
    io.open_output(Path, Result, !IO),
    (
        Result = ok(Stream),
        Strings = list.map(genotype_to_string, Genotypes),
        io.write_list(Stream, Strings, "\n", io.write_string(Stream), !IO),
        io.nl(Stream, !IO),
        io.close_output(Stream, !IO)
    ;
        Result = error(ErrorCode),
        io.error_message(ErrorCode, ErrorMessage),
        require.error(ErrorMessage)
    ).

genotype_to_string(Genotype) = String :-
    set.to_sorted_list(Genotype, List),
    String = string.join_list(" ", List).

%---------------------------------------------------------------------------%
