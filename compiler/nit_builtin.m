%-----------------------------------------------------------------------------%
%
% Main author: fjh.
%
% This file should be listed on the command line for nit before any of
% the others.
% This is done automatically by the Makefile when you do 'make foo.nit'.
%
%-----------------------------------------------------------------------------%

% Declare the appropriate operators.

:- op(1199, fx, (module)).
:- op(1199, fx, (end_module)).

:- op(1199, fx, (export_module)).
:- op(1199, fx, (export_sym)).
:- op(1199, fx, (export_pred)).
:- op(1199, fx, (export_cons)).
:- op(1199, fx, (export_type)).
:- op(1199, fx, (export_adt)).
:- op(1199, fx, (export_op)).

:- op(1199, fx, (import_module)).
:- op(1199, fx, (import_sym)).
:- op(1199, fx, (import_pred)).
:- op(1199, fx, (import_cons)).
:- op(1199, fx, (import_type)).
:- op(1199, fx, (import_adt)).
:- op(1199, fx, (import_op)).

:- op(1199, fx, (use_module)).
:- op(1199, fx, (use_sym)).
:- op(1199, fx, (use_pred)).
:- op(1199, fx, (use_cons)).
:- op(1199, fx, (use_type)).
:- op(1199, fx, (use_adt)).
:- op(1199, fx, (use_op)).

:- op(1199, fx, (rule)).

:- op(1199, fx, (mode)).
:- op(1199, fx, (inst)).
:- op(1179, xfy, (--->)).
:- op(1175, xfx, (::)).

% Give nit's "void" type a more meaningful name.

:- type (pred) == void.

% allow nit to ignore the modes in a pred declaration `:- pred p(T::M)'.

:- type (Type :: Mode) == Type.

% nit detects undefined types even if they're not used
% so we need to give type definitions for all the modes
% to prevent warnings about undefined types

:- type input == any.
:- type output == any.
:- type i == any.
:- type o == any.
:- type a == any.
:- type ui == any.
:- type uo == any.
:- type di == any.
:- type wi == any.
:- type wo == any.
:- type (_ -> _) == any.
:- type ground == any.
:- type free_unique == any.
:- type ground_unique == any.

:- type int == integer.

%-----------------------------------------------------------------------------%

% define the `character' type (this is builtin in Mercury, but not in
% NU-Prolog).

:- type character 	--->	
			%%%	('\000')	% not supported by NU-Prolog
				('\001')
			;	('\002')
			;	('\003')
			;	('\004')
			;	('\005')
			;	('\006')
			;	('\007')
			;	('\010')
			;	('\011')
			;	('\012')
			;	('\013')
			;	('\014')
			;	('\015')
			;	('\016')
			;	('\017')
			;	('\020')
			;	('\021')
			;	('\022')
			;	('\023')
			;	('\024')
			;	('\025')
			;	('\026')
			;	('\027')
			;	('\030')
			;	('\031')
			;	('\032')
			;	('\033')
			;	('\034')
			;	('\035')
			;	('\036')
			;	('\037')
			;	('\040')
			;	('\041')
			;	('\042')
			;	('\043')
			;	('\044')
			;	('\045')
			;	('\046')
			;	('\047')
			;	('\050')
			;	('\051')
			;	('\052')
			;	('\053')
			;	('\054')
			;	('\055')
			;	('\056')
			;	('\057')
			;	('\060')
			;	('\061')
			;	('\062')
			;	('\063')
			;	('\064')
			;	('\065')
			;	('\066')
			;	('\067')
			;	('\070')
			;	('\071')
			;	('\072')
			;	('\073')
			;	('\074')
			;	('\075')
			;	('\076')
			;	('\077')
			;	('\100')
			;	('\101')
			;	('\102')
			;	('\103')
			;	('\104')
			;	('\105')
			;	('\106')
			;	('\107')
			;	('\110')
			;	('\111')
			;	('\112')
			;	('\113')
			;	('\114')
			;	('\115')
			;	('\116')
			;	('\117')
			;	('\120')
			;	('\121')
			;	('\122')
			;	('\123')
			;	('\124')
			;	('\125')
			;	('\126')
			;	('\127')
			;	('\130')
			;	('\131')
			;	('\132')
			;	('\133')
			;	('\134')
			;	('\135')
			;	('\136')
			;	('\137')
			;	('\140')
			;	('\141')
			;	('\142')
			;	('\143')
			;	('\144')
			;	('\145')
			;	('\146')
			;	('\147')
			;	('\150')
			;	('\151')
			;	('\152')
			;	('\153')
			;	('\154')
			;	('\155')
			;	('\156')
			;	('\157')
			;	('\160')
			;	('\161')
			;	('\162')
			;	('\163')
			;	('\164')
			;	('\165')
			;	('\166')
			;	('\167')
			;	('\170')
			;	('\171')
			;	('\172')
			;	('\173')
			;	('\174')
			;	('\175')
			;	('\176')
			;	('\177').

%-----------------------------------------------------------------------------%
