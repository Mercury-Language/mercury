%---------------------------------------------------------------------------%
% Copyright (C) 1998-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module browser_library.

:- interface.

:- pred browser_library__version(string::out) is det.

:- implementation.

:- import_module browse, frame, help, parse, util.
:- import_module debugger_interface.
:- import_module declarative_debugger, declarative_oracle, declarative_user.
:- import_module interactive_query, dl, name_mangle.
:- import_module collect_lib.

% See library/library.m for why we implement this predicate this way.

:- pragma c_code(browser_library__version(Version::out),
		will_not_call_mercury, "
	ConstString version_string = 
		MR_VERSION "", configured for "" MR_FULLARCH;
	/*
	** Cast away const needed here, because Mercury declares Version
	** with type String rather than ConstString.
	*/
	Version = (String) (Word) version_string;
").

%---------------------------------------------------------------------------%
