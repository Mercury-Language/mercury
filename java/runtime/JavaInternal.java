//
// Copyright (C) 2001-2002 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//
//
// All modifications to this file will require changes to:
// compiler/mlds_to_java.m
// 
//
// At the moment this class is just used to store the command line
//   arguments.  We can't put them in one of the library modules because
//   we need to hold them in a class variable in a top level class.   
//

package mercury.runtime;

public class JavaInternal {
	public static java.lang.String[] args;
}
