%------------------------------------------------------------------------------%
% Copyright (C) 1999 INRIA/INSA.
% 
% Authors : Erwan Jahier <jahier@irisa.fr>,
%           Mireille Ducassé <ducasse@irisa.fr>
% 
% This file is compiled from make_scenario.pl and load_scenario.pl


% to avoid seeing the singleton variable checkings
:- set_flag(variable_names, on).

:- import set_opium_level/1 from sepia_kernel. 

:- get_flag(prolog_suffix, S), set_flag(prolog_suffix, [".op" | S]).

% to initialize module 'Opium-M'

:- op(500, fx, =).
:- op(500, fx, <).
:- op(500, fx, =<).
:- op(500, fx, >).
:- op(500, fx, >=).

:- dynamic opium_command/10.
:- dynamic opium_parameter/8.
:- dynamic opium_primitive/7.
:- dynamic opium_procedure/6.
:- dynamic opium_scenario/6.
:- dynamic opium_type/4.
:- dynamic opium_demo/5.
:- dynamic autoload_command/2.
:- dynamic autoload_scenario/4.

:- dynamic opium_command/9.
:- dynamic opium_parameter/6.
:- dynamic opium_primitive/6.
:- dynamic opium_procedure/5.
:- dynamic opium_scenario/4.
:- dynamic opium_type/3.
:- dynamic opium_demo/3.
:- dynamic autoload_command/2.
:- dynamic autoload_scenario/4.


opium_module.


/* mandatory for bootstrapping */

/*  to avoid that file is dumped in compiled query
 */

mycompile(F) :-
	compile(F).




/*
 *  link commands/procedures to implementations to enable the bootstrapping 
 *  before the scenario handler links together commands/procedures and their
 *  implementations
 */

make(S, MOD, OL, SD, OD) :- make_scenario_Op(S, MOD, OL, SD, OD).
opium_scenario_in_module(S, M) :- opium_scenario_in_module_Op(S, M).
set_default_parameters_in_module(S, Mod) :- set_default_parameters_in_module_Op(S, Mod).
check_arg_type(X, Y, Z, T, M) :- check_arg_type_Op(X, Y, Z, T, M).
check_arg(X, Y, Z, T, M) :- check_arg_Op(X, Y, Z, T, M).
modify_time(F, T) :- modify_time_Op(F, T).

is_list(X) :- is_list_Op(X).
is_list_of_atoms(X) :- is_list_of_atoms_Op(X).
is_list_of_atoms_or_empty_list(X) :- is_list_of_atoms_or_empty_list_Op(X).
is_list_of_vars_or_empty_list(X) :- is_list_of_vars_or_empty_list_Op(X).
is_list_of_ports(X) :- is_list_of_ports_Op(X).
is_opium_declaration(P/A) :- is_opium_declaration_Op(P/A).
is_opium_module(M) :- is_opium_module_Op(M).
opium_module(M) :- opium_module_Op(M).

% interface_status(X) :- interface_status_Op(X).

opium_write(V, M) :- opium_write_Op(V, M).
opium_printf(V, F, A) :- opium_printf_Op(V, F, A).
opium_printf(V, F, A, S) :- opium_printf_Op(V, F, A, S).
opium_nl(V) :- opium_nl_Op(V).


get_opium_file("opium_module", File) :-
	getenv('MERCURY_OPIUM_DIR', Path),
	append_strings(Path, "source/opium_module.sd", File).


/*
 *  sprintf/3 
 *  the formatted string is converted to an atom an 
 *  instantiated to the first parameter
 */
sprintf(Atom, Format, List) :-
	open(_, string, Stream),
	printf(Stream, Format, List),
	current_stream(String, _, Stream),
	atom_string(Atom, String),
	close(Stream).

/*
 *  namevar/2
 *  returns the name of a sepia variable as atom
 */
namevar(V, VN) :-
	var(V),
	open(_, string, Stream),
	printf(Stream, "%QDw", [V]),
	current_stream(S, _, Stream),
	atom_string(VN, S),
	close(Stream).

opium_level(0).

build_obj_dir(OD) :-
	getcwd(Cwd),
	append_strings(Cwd, "opiumfiles/", ODS),
	atom_string(OD, ODS).


% To be able to read Mercury terms, we need to set the associativities and
% precedences according to what is done in Mercury (taken from 
% mercury/library/ops.m).
set_mercury_assoc :-
	op(1025, xfy, '&'),		% Mercury extension
	op(1179, xfy, '--->'),		% Mercury extension
	op(600, yfx, ':'),		% `xfy' in ISO Prolog
	op(1175, xfx, '::'),		% Mercury extension
	op(920, xfy, '<='),		% Mercury/NU-Prolog extension
	op(920, xfy, '<=>'),		% Mercury/NU-Prolog extension
	op(920, xfy, '=>'),		% Mercury/NU-Prolog extension
	% XXX produce an `out of range' Error in Eclipse.
	% op(950, fxy, 'all'),		% Mercury/NU-Prolog extension
	op(1170, xfy, 'else'),		% Mercury/NU-Prolog extension
	op(1199, fx, 'end_module'),	% Mercury extension
	op(1199, fx, 'export_adt'),	% Mercury extension (NYI)
	op(1199, fx, 'export_cons'),	% Mercury extension (NYI)
	op(1199, fx, 'export_module'),	% Mercury extension (NYI)
	op(1199, fx, 'export_op'),	% Mercury extension (NYI)
	op(1199, fx, 'export_pred'),	% Mercury extension (NYI)
	op(1199, fx, 'export_sym'),	% Mercury extension (NYI)
	op(1199, fx, 'export_type'),	% Mercury extension (NYI)
	op(800, fx, 'func'),		% Mercury extension
	op(1160, fx, 'if'),		% Mercury/NU-Prolog extension
	op(1199, fx, 'import_adt'),	% Mercury extension (NYI)
	op(1199, fx, 'import_cons'),	% Mercury extension (NYI)
	op(1199, fx, 'import_module'),	% Mercury extension
	op(1199, fx, 'include_module'), % Mercury extension
	op(1199, fx, 'import_op'),	% Mercury extension (NYI)
	op(1199, fx, 'import_pred'),	% Mercury extension (NYI)
	op(1199, fx, 'import_sym'),	% Mercury extension (NYI)
	op(1199, fx, 'import_type'),	% Mercury extension (NYI)
	op(800, fy, 'impure'),		% Mercury extension
	op(1199, fx, 'inst'),		% Mercury extension
	op(1199, fx, 'instance'),	% Mercury extension
	op(701, xfx, 'is'),		% ISO Prolog says prec 700
	% XXX produce an `out of range' Error in Eclipse.
	% op(950, fxy, 'lambda'),	% Mercury extension
	op(1199, fx, 'mode'),		% Mercury extension
	op(1199, fx, 'module'),		% Mercury extension
	op(900, fy, 'not'),		% Mercury/NU-Prolog extension
	op(800, fx, 'pragma'),		% Mercury extension
	op(800, fx, 'pred'),		% Mercury/NU-Prolog extension
	op(800, fy, 'semipure'),	% Mercury extension
	% XXX produce an `out of range' Error in Eclipse.
	% op(950, fxy, 'some'),		% Mercury/NU-Prolog extension
	op(1150, xfx, 'then'),		% Mercury/NU-Prolog extension
	op(1180, fx, 'type'),		% Mercury extension
	op(1199, fx, 'typeclass'),	% Mercury extension
	op(1199, fx, 'use_adt'),	% Mercury extension (NYI)
	op(1199, fx, 'use_cons'),	% Mercury extension (NYI)
	op(1199, fx, 'use_module'),	% Mercury extension (NYI)
	op(1199, fx, 'use_op'),		% Mercury extension (NYI)
	op(1199, fx, 'use_pred'),	% Mercury extension (NYI)
	op(1199, fx, 'use_sym'),	% Mercury extension (NYI)
	op(1199, fx, 'use_type').	% Mercury extension (NYI)

reset_mercury_assoc :-
	abolish_op('&', xfy),           % Mercury extension
	abolish_op('--->', xfy),        % Mercury extension
	abolish_op(':', yfx),		% `xfy' in ISO Prolog
	abolish_op('::', xfx),          % Mercury extension
	abolish_op('<=', xfy),          % Mercury/NU-Prolog extension
	abolish_op('<=>', xfy),         % Mercury/NU-Prolog extension
	abolish_op('=>', xfy),          % Mercury/NU-Prolog extension
	%abolish_op('all', fxy),        % Mercury/NU-Prolog extension
	abolish_op('else', xfy),        % Mercury/NU-Prolog extension
	abolish_op('end_module', fx),   % Mercury extension
	abolish_op('export_adt', fx),   % Mercury extension (NYI)
	abolish_op('export_cons', fx),  % Mercury extension (NYI)
	abolish_op('export_module', fx),% Mercury extension (NYI)
	abolish_op('export_op', fx),    % Mercury extension (NYI)
	abolish_op('export_pred', fx),  % Mercury extension (NYI)
	abolish_op('export_sym', fx),   % Mercury extension (NYI)
	abolish_op('export_type', fx),  % Mercury extension (NYI)
	abolish_op('func', fx),         % Mercury extension
	abolish_op('if', fx),           % Mercury/NU-Prolog extension
	abolish_op('import_adt', fx),   % Mercury extension (NYI)
	abolish_op('import_cons', fx),  % Mercury extension (NYI)
	abolish_op('import_module', fx),% Mercury extension
	abolish_op('include_module', fx),% Mercury extension
	abolish_op('import_op', fx),    % Mercury extension (NYI)
	abolish_op('import_pred', fx),  % Mercury extension (NYI)
	abolish_op('import_sym', fx),   % Mercury extension (NYI)
	abolish_op('import_type', fx),  % Mercury extension (NYI)
	abolish_op('impure', fy),       % Mercury extension
	abolish_op('inst', fx),         % Mercury extension
	abolish_op('instance', fx),     % Mercury extension
	abolish_op('is', xfx),          % ISO Prolog says prec 700
	%abolish_op('lambda', fxy),     % Mercury extension
	abolish_op('mode', fx),         % Mercury extension
	abolish_op('module', fx),       % Mercury extension
	abolish_op('not', fy),          % Mercury/NU-Prolog extension
	abolish_op('pragma', fx),       % Mercury extension
	abolish_op('pred', fx),         % Mercury/NU-Prolog extension
	abolish_op('semipure', fy),     % Mercury extension
	%abolish_op('some', fxy),       % Mercury/NU-Prolog extension
	abolish_op('then', xfx),        % Mercury/NU-Prolog extension
	abolish_op('type', fx),         % Mercury extension
	abolish_op('typeclass', fx),    % Mercury extension
	abolish_op('use_adt', fx),      % Mercury extension (NYI)
	abolish_op('use_cons', fx),     % Mercury extension (NYI)
	abolish_op('use_module', fx),   % Mercury extension (NYI)
	abolish_op('use_op', fx),       % Mercury extension (NYI)
	abolish_op('use_pred', fx),     % Mercury extension (NYI)
	abolish_op('use_sym', fx),      % Mercury extension (NYI)
	abolish_op('use_type', fx).     % Mercury extension (NYI)


read_mercury_term(S, Term) :-
	set_mercury_assoc,
	read(S, Term),
	reset_mercury_assoc.

read_mercury_term(Term) :-
	set_mercury_assoc,
	read(Term),
	reset_mercury_assoc.

write_mercury_term(Term) :-
	set_mercury_assoc,
	write(Term),
	reset_mercury_assoc.
