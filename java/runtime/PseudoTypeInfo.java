//
// Copyright (C) 2001-2004 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

// A PseudoTypeInfo represents a possibly non-ground type.
// There are three possible cases:
//
//   - Unbound type variables are represented by directly constructing
//     a PseudoTypeInfo with the variable number.
//
//   - Ground types with arity zero may be represented as TypeCtorInfo_Struct,
//     which extends PseudoTypeInfo, and uses the protected constructor
//     which sets variable_number to -1.  This is a slightly optimized
//     version of the case below.
//
//   - Any other types are represented as TypeInfo_Struct,
//     which extends PseudoTypeInfo, and uses the protected constructor
//     which sets variable_number to -1.
//
public class PseudoTypeInfo {
	public int variable_number;
	public    PseudoTypeInfo(int n) { variable_number = n; }
	protected PseudoTypeInfo()      { variable_number = -1; }
}
