%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006, 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: evolve.m.
% Main author: samrith.
%
% This program implements part of a genetic algorithm to determine an
% optimal set of optimisation flags to be passed to the Mercury compiler
% for a given program.
%
% It expects to be given two pieces of data as input.  The first is a
% list of genotypes, where each genotype is a set of strings representing
% optimisation flags.  The second is a list of phenotypes, where each
% phenotype is a list of benchmarks.  These are read from the files
% generations/$n/genotypes and generations/$n/phenotypes, respectively.
%
% The program will then determine the next set of genotypes, which it
% will write in the file generations/$n+1/genotypes.  It will also create
% the file generations/$n/ladder, which contains a table with all of the
% genotypes and their fitness values.
%
% Note that this program does not perform the actual benchmarking tests,
% nor does it control the evolution over multiple generations.  These
% tasks are handled by the evaluate and gator scripts, respectively.
%
%-----------------------------------------------------------------------------%

:- module evolve.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module genotype.
:- import_module phenotype.
:- import_module tausworthe3.

:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module std_util.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    % Process any command line arguments.
    io.command_line_arguments(Args, !IO),
    OptionOps = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOps, Args, _, Result),
    (
        Result = ok(OptionTable),

        getopt.lookup_string_option(OptionTable, config_file,
            PathToConfigFile),
        getopt.lookup_string_option(OptionTable, genotypes, PathToGenotypes),
        getopt.lookup_string_option(OptionTable, next_genotypes,
            PathToNextGenotypes),
        getopt.lookup_string_option(OptionTable, ladder, PathToLadder),
        getopt.lookup_string_option(OptionTable, phenotypes, PathToPhenotypes),

        getopt.lookup_int_option(OptionTable, first_seed, FirstSeed),
        getopt.lookup_int_option(OptionTable, second_seed, SecondSeed),
        getopt.lookup_int_option(OptionTable, third_seed, ThirdSeed)
    ;
        Result = error(Error),
        require.error(option_error_to_string(Error))
    ),

    % Read the input files.
    read_config_file(PathToConfigFile, Weightings, Flags, !IO),
    genotype.read_genotypes(PathToGenotypes, Genotypes, !IO),
    phenotype.read_phenotypes(PathToPhenotypes, Phenotypes, !IO),

    % Apply the genetic operators to the genotypes.
    some [!RNG] (
        Tausworthe3Seed = tausworthe3_seed(FirstSeed, SecondSeed, ThirdSeed),
        !:RNG = init_tausworthe3,
        seed(Tausworthe3Seed, !RNG),

        % We ensure that the following condition holds:
        %
        %   length(Genotypes) / 2 = length(Mothers) = length(Fathers).
        %
        % Since the crossover/6 predicate creates two children for
        % every two parents, the population will remain constant
        % over generations. This is a simple way of avoiding extinction.
        %
        % Note that map_2in_2out_foldl/7 will simply ignore leftover elements
        % if the two lists (Mothers and Fathers) are of unequal lengths.
        %
        Fitness = list.map(phenotype.fitness(Weightings), Phenotypes),
        list.map_foldl(phenotype.selection(Genotypes, Fitness),
            Genotypes, Parents, !RNG),
        list.det_split_list(length(Parents) / 2, Parents, Mothers, Fathers),
        map_2in_2out_foldl(genotype.crossover, Mothers, Fathers,
            Sons, Daughters, !RNG),
        list.append(Sons, Daughters, Children),
        list.map_foldl(genotype.mutation(Flags), Children, NextGenotypes,
            !.RNG, _)
    ),

    % Print the output files.
    genotype.print_genotypes(PathToNextGenotypes, NextGenotypes, !IO),
    print_ladder(PathToLadder, Fitness, Genotypes, !IO).

%-----------------------------------------------------------------------------%
%
% Command line argument parsing.
%
% This section contains all the code for the predicates required by
% getopt.process_options.
%

:- type option
    --->    config_file
    ;       genotypes
    ;       next_genotypes
    ;       ladder
    ;       phenotypes
    ;       first_seed
    ;       second_seed
    ;       third_seed.

:- pred short_option(char::in, option::out) is semidet.

short_option('c', config_file).
short_option('g', genotypes).
short_option('h', next_genotypes).
short_option('l', ladder).
short_option('p', phenotypes).
short_option('s', first_seed).
short_option('t', second_seed).
short_option('u', third_seed).

:- pred long_option(string::in, option::out) is semidet.

long_option("config-file", config_file).
long_option("genotypes", genotypes).
long_option("next-genotypes", next_genotypes).
long_option("ladder", ladder).
long_option("phenotypes", phenotypes).
long_option("first-seed", first_seed).
long_option("second-seed", second_seed).
long_option("third-seed", third_seed).

:- pred option_default(option::out, option_data::out) is multi.

option_default(config_file, string("evolve.conf")).
option_default(genotypes, string("generations/1/genotypes")).
option_default(next_genotypes, string("generations/2/genotypes")).
option_default(ladder, string("generations/1/ladder")).
option_default(phenotypes, string("generations/1/phenotypes")).
option_default(first_seed, int(0)).
option_default(second_seed, int(0)).
option_default(third_seed, int(0)).

%-----------------------------------------------------------------------------%
%
% Code for reading configuration files.
%

:- pred read_config_file(string::in, list(weighting)::out, list(flag)::out,
    io::di, io::uo) is det.

read_config_file(Path, Weightings, Flags, !IO) :-
    io.open_input(Path, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        io.read(Stream, ReadWeightingsResult, !IO),
        (
            ReadWeightingsResult = ok(Weightings),
            io.read(Stream, ReadFlagsResult, !IO),
            (
                ReadFlagsResult = ok(Flags),
                io.close_input(Stream, !IO)
            ;
                ReadFlagsResult = eof,
                require.error("unexpected EOF")
            ;
                ReadFlagsResult = error(ErrorMsg, LineNum),
                string.format("%d: %s", [i(LineNum), s(ErrorMsg)], Message),
                require.error(Message)
            )
        ;
            ReadWeightingsResult = eof,
            require.error("unexpected EOF")
        ;
            ReadWeightingsResult = error(ErrorMsg, LineNum),
            string.format("%d: %s", [i(LineNum), s(ErrorMsg)], Message),
            require.error(Message)
        )
    ;
        OpenResult = error(ErrorCode),
        io.error_message(ErrorCode, ErrorMessage),
        require.error(ErrorMessage)
    ).

:- pred print_ladder(string::in, list(fitness)::in, list(genotype)::in,
    io::di, io::uo) is det.

print_ladder(Path, Fitness, Genotypes, !IO) :-
    io.open_output(Path, OpenResult, !IO),
    (
        OpenResult = ok(Stream),

        list.map(string.int_to_string, 1..list.length(Fitness), C1),
        C2 = list.map(phenotype.fitness_to_string, Fitness),
        C3 = list.map(genotype.genotype_to_string, Genotypes),

        Table = string.format_table([right(C1), right(C2), left(C3)], " * "),
        io.write_string(Stream, Table, !IO),

        io.close_output(Stream, !IO)
    ;
        OpenResult = error(ErrorCode),
        io.error_message(ErrorCode, ErrorMessage),
        require.error(ErrorMessage)
    ).

%-----------------------------------------------------------------------------%
%
% Miscellaneous.
%

    % map_2in_2out_foldl(Pred, InList1, InList2, OutList1, OutList2, !A).
    %
    % This predicate is the same as list.map_foldl, except that it takes
    % two input lists and two output lists.
    %
:- pred map_2in_2out_foldl(
    pred(L, M, N, O, A, A)::(pred(in, in, out, out, in, out) is det),
    list(L)::in, list(M)::in, list(N)::out, list(O)::out, A::in, A::out)
    is det.

map_2in_2out_foldl(_, [],        [],        [],        [],        !A).
map_2in_2out_foldl(_, [],        [_H | _T], [],        [],        !A).
map_2in_2out_foldl(_, [_H | _T], [],        [],        [],        !A).
map_2in_2out_foldl(P, [H0 | T0], [H1 | T1], [H2 | T2], [H3 | T3], !A) :-
    P(H0, H1, H2, H3, !A),
    map_2in_2out_foldl(P, T0, T1, T2, T3, !A).

%-----------------------------------------------------------------------------%
