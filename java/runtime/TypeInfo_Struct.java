// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2001-2004, 2011 The University of Melbourne.
// Copyright (C) 2014, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

public class TypeInfo_Struct extends PseudoTypeInfo
    implements java.io.Serializable
{

    public TypeCtorInfo_Struct type_ctor;
    public PseudoTypeInfo args[];

    public TypeInfo_Struct()
    {
    }

    public TypeInfo_Struct(TypeCtorInfo_Struct tc)
    {
        type_ctor = tc;
        sanity_check();
    }

    public static TypeInfo_Struct maybe_new(final TypeInfo_Struct ti)
    {
        return ti;
    }

    public static TypeInfo_Struct maybe_new(final TypeCtorInfo_Struct tci)
    {
        return new TypeInfo_Struct(tci);
    }

    public static TypeInfo_Struct maybe_new(final PseudoTypeInfo obj)
    {
        if (obj instanceof TypeCtorInfo_Struct) {
            return new TypeInfo_Struct((TypeCtorInfo_Struct) obj);
        }
        return (TypeInfo_Struct) obj;
    }

    public void init(TypeCtorInfo_Struct tc, PseudoTypeInfo[] as)
    {
        type_ctor = tc;
        args = as;

        // We may be in the middle of initialising a cyclic data structure,
        // so unfortunately, we can't actually sanity check the arguments here.
        assert tc != null;
        // sanity_check();
    }

    public TypeInfo_Struct copy()
    {
        TypeInfo_Struct ti = new TypeInfo_Struct();
        ti.type_ctor = type_ctor;
        if (args != null) {
            ti.args = args.clone();
        }
        ti.sanity_check();
        return ti;
    }

    // XXX "as" should have type PseudoTypeInfo[],
    //     but mlds_to_java.m uses Object[]
    //     because init_array/1 does not store the type.
    public void init(TypeCtorInfo_Struct tc, int arity, Object[] as)
    {
        assert arity == as.length;

        init(tc, as);
    }

    // XXX "as" should have type PseudoTypeInfo[],
    //     but mlds_to_java.m uses Object[]
    //     because init_array/1 does not store the type.
    public void init(TypeCtorInfo_Struct tc, Object[] as)
    {
        PseudoTypeInfo[] ptis = new PseudoTypeInfo[as.length];
        for (int i = 0; i < as.length; i++) {
            ptis[i] = (PseudoTypeInfo) as[i];
        }
        init(tc, ptis);
    }

    // We don't have a version of this constructor that also takes the arity
    // as an argument (as we do with the init method above), e.g.
    //
    //  public TypeInfo_Struct(TypeInfo_Struct ti, int artiy, Object... as)
    //
    // because such overloadings are not allowed under Java 1.7.
    // (Previous versions of Java incorrectly allowed them.) This is okay,
    // because init above simply checks the number of items in the array
    // to determine the arity.
    //
    // If you change this you will also need to update the code in
    // compiler/rtti_to_mlds.m and compiler/polymorphism.m
    //
    public TypeInfo_Struct(Object a1, Object... as)
    {
        if (a1 instanceof TypeInfo_Struct) {
            TypeInfo_Struct ti = (TypeInfo_Struct)a1;
            init(ti.type_ctor, as);
        } else if (a1 instanceof TypeCtorInfo_Struct) {
            init((TypeCtorInfo_Struct)a1, as);
        } else {
            throw new ClassCastException(
                "cannot cast argument to a TypeInfo or " +
                "TypeCtorInfo");
        }
    }

    // XXX a temp hack just to get things to run
    public TypeInfo_Struct(java.lang.Object obj)
    {
        if (obj instanceof TypeInfo_Struct) {
            TypeInfo_Struct ti = (TypeInfo_Struct) obj;
            type_ctor = ti.type_ctor;
            args = ti.args;
        } else {
            throw new java.lang.Error("TypeInfo_Struct(Object)");
        }
    }

    // XXX this should be renamed `equals'
    public boolean unify(TypeInfo_Struct ti) {
        if (this == ti) {
            return true;
        }

        TypeInfo_Struct self = this.collapse_equivalences();
        ti = ti.collapse_equivalences();

        if (self == ti) {
            return true;
        }

        if (!self.type_ctor.unify(ti.type_ctor)) {
            return false;
        }

        int len1 = 0;
        int len2 = 0;
        if (self.args != null) {
            len1 = self.args.length;
        }
        if (ti.args != null) {
            len2 = ti.args.length;
        }
        if (len1 != len2) {
            return false;
        }

        for (int i = 0; i < len1; i++) {
            if (!self.args[i].unify(ti.args[i])) {
                return false;
            }
        }
        return true;
    }

    private TypeInfo_Struct collapse_equivalences() {
        TypeInfo_Struct ti = this;

        // Look past equivalences.
        while (ti.type_ctor.type_ctor_rep.value ==
                TypeCtorRep.MR_TYPECTOR_REP_EQUIV_GROUND
            || ti.type_ctor.type_ctor_rep.value ==
                TypeCtorRep.MR_TYPECTOR_REP_EQUIV)
        {
            ti = TypeInfo_Struct.maybe_new(
                ti.type_ctor.type_layout.layout_equiv());
        }

        return ti;
    }

    private void sanity_check() {
        assert type_ctor != null;

        if (args == null) {
            return;
        }
        for (PseudoTypeInfo pti : args) {
            if (pti instanceof TypeInfo_Struct) {
                TypeInfo_Struct ti = (TypeInfo_Struct) pti;
                assert ti.type_ctor != null;
                assert ti.variable_number == -1;
            } else if (pti instanceof TypeCtorInfo_Struct) {
                TypeCtorInfo_Struct tc =
                    (TypeCtorInfo_Struct) pti;
                assert tc.variable_number == -1;
            } else {
                assert pti.variable_number != -1;
            }
        }
    }
}
