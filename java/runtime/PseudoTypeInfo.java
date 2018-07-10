// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2001-2004, 2009 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

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
public class PseudoTypeInfo implements java.io.Serializable {
    public int variable_number;
    public    PseudoTypeInfo(int n) { variable_number = n; }
    protected PseudoTypeInfo()      { variable_number = -1; }

    // Adding or removing members requires corresponding changes in
    // mlds_to_java.m.
    public static final PseudoTypeInfo K1 = new PseudoTypeInfo(1);
    public static final PseudoTypeInfo K2 = new PseudoTypeInfo(2);
    public static final PseudoTypeInfo K3 = new PseudoTypeInfo(3);
    public static final PseudoTypeInfo K4 = new PseudoTypeInfo(4);
    public static final PseudoTypeInfo K5 = new PseudoTypeInfo(5);

    // XXX This should be renamed `equals'
    public boolean unify(PseudoTypeInfo ti) {
        if (this.getClass() == TypeInfo_Struct.class &&
                ti.getClass() == TypeInfo_Struct.class) {
            return ((TypeInfo_Struct) this).unify(
                        (TypeInfo_Struct) ti);
        }
        return variable_number == ti.variable_number;
    }
}
