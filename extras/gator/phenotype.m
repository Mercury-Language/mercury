%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006, 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: phenotype.m.
% Main author: samrith.
%
%-----------------------------------------------------------------------------%

:- module phenotype.
:- interface.

:- import_module genotype.
:- import_module tausworthe3.

:- import_module io.
:- import_module list.

:- type phenotype.

:- type weighting.

:- type fitness.

    % read_phenotypes(Path, Phenotypes, !IO):
    %
    % Reads a list of phenotypes from the given file.
    %
:- pred read_phenotypes(string::in, list(phenotype)::out, io::di, io::uo)
    is det.

    % fitness(Weightings, Phenotype) = Fitness:
    %
    % This function evaluates the fitness of a genotype given its phenotype.
    %
    % Fitness is defined here as a weighted sum of each number in the
    % phenotype. The weightings are given by Weightings.
    %
:- func fitness(list(weighting), phenotype) = fitness is det.

    % selection(Genotypes, Fitness, _, Parent, !RNG):
    %
    % This predicate randomly selects an individual for reproduction.
    %
    % I've considered fitness proportionate selection (aka roulette
    % wheel selection) and tournament selection. The fitness
    % proportionate selection method will be used as it is simple and
    % doesn't require us to tweak any parameters (which could require a
    % lot of time-consuming experimentation).
    %
    % The third argument is ignored. It is just there to make it easy
    % to control the number of parents to be selected.
    %
:- pred selection(list(genotype)::in, list(fitness)::in, T::in,
        genotype::out, RNG::in, RNG::out) is det <= random(RNG, Seed).

    % fitness_to_string(Fitness) = String.
    %
    % Gives a string representation of the fitness value.
    %
:- func fitness_to_string(fitness) = string is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module require.
:- import_module std_util.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type phenotype
    --->    phenotype(
                compile_times       :: list(compile_time),
                executable_sizes    :: list(executable_size),
                run_times           :: list(run_time)
            ).

:- type compile_time == float.

:- type executable_size == int.

:- type run_time == float.

:- type weighting == float.

:- type fitness == float.

read_phenotypes(Path, Phenotypes, !IO) :-
    io.open_input(Path, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        io.read(Stream, ReadResult, !IO),
        (
            ReadResult = ok(Phenotypes),
            io.close_input(Stream, !IO)
        ;
            ReadResult = eof,
            require.error("unexpected EOF while reading phentoypes")
        ;
            ReadResult = error(ErrorMsg, LineNum),
            string.format("%d: %s", [i(LineNum), s(ErrorMsg)], Message),
            require.error(Message)
        )
    ;
        OpenResult = error(ErrorCode),
        io.error_message(ErrorCode, ErrorMessage),
        require.error(ErrorMessage)
    ).

fitness(Weightings, Phenotype) = Fitness :-

    CompileTimes = Phenotype ^ compile_times,
    ExecutableSizes = list.map(float.float, Phenotype ^ executable_sizes),
    RunTimes = Phenotype ^ run_times,

    list.condense([CompileTimes, ExecutableSizes, RunTimes], Benchmarks),
    map_2in_1out(*, Weightings, Benchmarks) = WeightedBenchmarks,
    list.foldl(+, WeightedBenchmarks, 0.0) = SumOfWeightedBenchmarks,
    
    Fitness = 1.0 / SumOfWeightedBenchmarks.

selection(Genotypes, Fitness, _, Parent, !RNG) :-
    % Normalise fitness values. The sum of all the normalised fitness values
    % should be equal to 1.0. This is achieved by dividing each of the
    % fitness values by the sum of the fitness values.
    list.foldl(+, Fitness, 0.0) = Sum,
    list.map(std_util.converse(/, Sum), Fitness) = NormalFitness,

    % Find the accumulated normalised fitness values. The accumulated
    % normalised fitness value is the sum of the normalised fitness values
    % for the current and all previous genotypes.
    list.map_foldl(pred(X::in, X + Y::out, Y::in, X + Y::out) is det,
            NormalFitness, CumulativeNormalFitness, 0.0, _),

    % Pick a random number between zero and one.
    next(Int, !RNG),
    max(Maximum, !RNG),
    Float = float(Int) / float(Maximum),

    % Find the first cumulative normalised fitness value that is greater than
    % or equal to the random number picked, and find its corresponding
    % genotype.
    %
    % Note that we're guaranteed to have at least one element in AfterList,
    % since the last element of CumulativeNormalFitness (and therefore
    % AfterList) is exactly 1.0, and Float is no greater than 1.0.
    %
    list.take_while(>(Float), CumulativeNormalFitness, _, AfterList),
    Head = list.det_head(AfterList),
    Index = list.det_index0_of_first_occurrence(CumulativeNormalFitness, Head),
    list.det_index0(Genotypes, Index, Parent).

fitness_to_string(Fitness) = string.float_to_string(Fitness).

%-----------------------------------------------------------------------------%
%
% Miscellaneous.
%

    % map_2in_1out(T, L, M) = N:
    %
    % This function is the same as list.map2/4, except that it takes two
    % input lists. Obviously it is also a function, unlike list.map2/4.
    %
:- func map_2in_1out(func(L, M) = N, list(L), list(M)) = list(N).
:- mode map_2in_1out(in, in, in) = out is det.

map_2in_1out(_, [],        []       ) = [].
map_2in_1out(_, [],        [_H | _T]) = [].
map_2in_1out(_, [_H | _T], []       ) = [].
map_2in_1out(F, [H0 | T0], [H1 | T1]) = [H2 | T2] :-
    F(H0, H1) = H2,
    map_2in_1out(F, T0, T1) = T2.

%-----------------------------------------------------------------------------%
