// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2001-2003 The University of Melbourne.
// Copyright (C) 2018, 2021 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

public class DuPtagLayout implements java.io.Serializable {

    public int sectag_sharers;
    public Sectag_Locn sectag_locn;
    public /* final */ DuFunctorDesc[] sectag_alternatives;
    public byte sectag_numbits;         // not used in Java grades
    public byte du_ptag;
    public byte du_ptag_flags;

    private final byte MR_DU_PTAG_FLAG_SECTAG_ALTERNATIVES_INDEXABLE = 0x1;

    public DuPtagLayout(
        int sectag_sharers,
        Sectag_Locn sectag_locn,
        DuFunctorDesc[] sectag_alts,
        byte sectag_numbits,
        byte du_ptag,
        byte du_ptag_flags)
    {
        this.sectag_sharers = sectag_sharers;
        this.sectag_locn = sectag_locn;
        this.sectag_alternatives = sectag_alts;
        this.sectag_numbits = sectag_numbits;
        this.du_ptag = du_ptag;
        this.du_ptag_flags = du_ptag_flags;
    }

    public DuPtagLayout(
        int sectag_sharers,
        int sectag_locn,
        DuFunctorDesc[] sectag_alts,
        byte sectag_numbits,
        byte du_ptag,
        byte du_ptag_flags)
    {
        this.sectag_sharers = sectag_sharers;
        this.sectag_locn = new Sectag_Locn(sectag_locn);
        this.sectag_alternatives = sectag_alts;
        this.sectag_numbits = sectag_numbits;
        this.du_ptag = du_ptag;
        this.du_ptag_flags = du_ptag_flags;
    }

    public DuFunctorDesc index_or_search_sectag_functor(int sectag) {
        if (flags_is_sectag_alternatives_indexable()) {
            return sectag_alternatives[sectag];
        }

        for (DuFunctorDesc desc : sectag_alternatives) {
            if (desc.du_functor_secondary == sectag) {
                return desc;
            }
        }

        return null;
    }

    private boolean flags_is_sectag_alternatives_indexable() {
        return (du_ptag_flags & MR_DU_PTAG_FLAG_SECTAG_ALTERNATIVES_INDEXABLE)
                != 0;
    }
}
