%-----------------------------------------------------------------------------%

% Value_number.nl - optimization of straight-line LLDS code.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module value_number.		

:- interface.
:- import_module llds.

:- pred value_number__optimize(c_file, c_file).
:- mode value_number__optimize(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module code_util, map, bintree_set, string, list, require, std_util.

%-----------------------------------------------------------------------------%

	% Find straight-line code sequences and optimize them using
	% value numbering.

:- pred value_number__optimize(list(instruction), list(instruction)).
:- mode value_number__optimize(in, out) is det.

value_number__optimize(Instructions, Instructions).

:- end_module value_number.

%-----------------------------------------------------------------------------%
