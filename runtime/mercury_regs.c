// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1997, 2000-2001, 2004, 2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#include "mercury_imp.h"
#include "mercury_regs.h"

#include <stdio.h>

MR_Word *MR_sol_hp_var = NULL;
MR_Word *MR_min_hp_rec_var = NULL;
MR_Word *MR_min_sol_hp_rec_var = NULL;
MR_Word *MR_global_hp_var = NULL;

MR_Word         MR_real_r_reg_map[MR_MAX_REAL_R_REG] = MR_REAL_R_REG_MAP_BODY;

unsigned long   MR_num_uses[MR_MAX_REAL_R_REG + MR_NUM_SPECIAL_REG];

#ifdef MR_MEASURE_REGISTER_USAGE

#define MR_COUNT_FORMAT "\t%lu\n"

void
MR_print_register_usage_counts(void)
{
    int i;

    printf("register usage counts:\n");
    for (i = 1; i <= MR_MAX_REAL_R_REG; i++) {
        printf("r%d", i);
        printf(MR_COUNT_FORMAT, MR_num_uses[MR_R_SLOT(i)]);
    }

    printf("MR_succip");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_SI_SLOT]);
    printf("MR_hp");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_HP_SLOT]);
    printf("MR_sp");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_SP_SLOT]);
    printf("MR_curfr");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_CF_SLOT]);
    printf("MR_maxfr");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_MF_SLOT]);
    printf("MR_trail_ptr");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_TRAIL_PTR_SLOT]);
    printf("MR_ticket_counter");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_TICKET_COUNTER_SLOT]);
    printf("MR_ticket_high_water");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_TICKET_HIGH_WATER_SLOT]);
    printf("MR_sol_hp");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_SOL_HP_SLOT]);
    printf("MR_min_hp_rec");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_MIN_HP_REC_SLOT]);
    printf("MR_min_sol_hp_rec");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_MIN_SOL_HP_REC_SLOT]);
    printf("MR_global_hp");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_GLOBAL_HP_SLOT]);
    printf("MR_gen_next");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_GEN_NEXT_SLOT]);
    printf("MR_gen_stack");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_GEN_STACK_SLOT]);
    printf("MR_cut_next");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_CUT_NEXT_SLOT]);
    printf("MR_cut_stack");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_CUT_STACK_SLOT]);
    printf("MR_pneg_next");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_PNEG_NEXT_SLOT]);
    printf("MR_pneg_stack");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_PNEG_STACK_SLOT]);
    printf("MR_parent_sp");
    printf(MR_COUNT_FORMAT, MR_num_uses[MR_PARENT_SP_SLOT]);
}
#endif  // MR_MEASURE_REGISTER_USAGE

#ifdef  MR_VERIFY_FAKE_REGISTERS

#define MR_VERIFY_FORMAT    "\t%d\n"

void
MR_verify_fake_registers(void)
{
    int i;

    printf("register slots:\n");
    for (i = 1; i <= MR_MAX_REAL_R_REG; i++) {
        printf("r%d", i);
        printf(MR_VERIFY_FORMAT, MR_R_SLOT(i));
    }

    printf("r%d", MR_MAX_REAL_R_REG + 1);
    printf(MR_VERIFY_FORMAT, MR_MAX_REAL_R_REG + MR_NUM_SPECIAL_REG);

    printf("MR_succip");
    printf(MR_VERIFY_FORMAT, MR_SI_SLOT);
    printf("MR_hp");
    printf(MR_VERIFY_FORMAT, MR_HP_SLOT);
    printf("MR_sp");
    printf(MR_VERIFY_FORMAT, MR_SP_SLOT);
    printf("MR_curfr");
    printf(MR_VERIFY_FORMAT, MR_CF_SLOT);
    printf("MR_maxfr");
    printf(MR_VERIFY_FORMAT, MR_MF_SLOT);
    printf("MR_trail_ptr");
    printf(MR_VERIFY_FORMAT, MR_TRAIL_PTR_SLOT);
    printf("MR_ticket_counter");
    printf(MR_VERIFY_FORMAT, MR_TICKET_COUNTER_SLOT);
    printf("MR_ticket_high_water");
    printf(MR_VERIFY_FORMAT, MR_TICKET_HIGH_WATER_SLOT);
    printf("MR_sol_hp");
    printf(MR_VERIFY_FORMAT, MR_SOL_HP_SLOT);
    printf("MR_min_hp_rec");
    printf(MR_VERIFY_FORMAT, MR_MIN_HP_REC_SLOT);
    printf("MR_min_sol_hp_rec");
    printf(MR_VERIFY_FORMAT, MR_MIN_SOL_HP_REC_SLOT);
    printf("MR_global_hp");
    printf(MR_VERIFY_FORMAT, MR_GLOBAL_HP_SLOT);
    printf("MR_gen_next");
    printf(MR_VERIFY_FORMAT, MR_GEN_NEXT_SLOT);
    printf("MR_gen_stack");
    printf(MR_VERIFY_FORMAT, MR_GEN_STACK_SLOT);
    printf("MR_cut_next");
    printf(MR_VERIFY_FORMAT, MR_CUT_NEXT_SLOT);
    printf("MR_cut_stack");
    printf(MR_VERIFY_FORMAT, MR_CUT_STACK_SLOT);
    printf("MR_pneg_next");
    printf(MR_VERIFY_FORMAT, MR_PNEG_NEXT_SLOT);
    printf("MR_pneg_stack");
    printf(MR_VERIFY_FORMAT, MR_PNEG_STACK_SLOT);
    printf("MR_parent_sp");
    printf(MR_VERIFY_FORMAT, MR_PARENT_SP_SLOT);
}
#endif  // MR_VERIFY_FAKE_REGISTERS

MR_Word
MR_get_reg(int num)
{
    MR_restore_transient_registers();
    switch (num) {

        case 1:  return MR_r1;
        case 2:  return MR_r2;
        case 3:  return MR_r3;
        case 4:  return MR_r4;
        case 5:  return MR_r5;
        case 6:  return MR_r6;
        case 7:  return MR_r7;
        case 8:  return MR_r8;
        case 9:  return MR_r9;
        case 10: return MR_r10;
        case 11: return MR_r11;
        case 12: return MR_r12;
        case 13: return MR_r13;
        case 14: return MR_r14;
        case 15: return MR_r15;
        case 16: return MR_r16;
        case 17: return MR_r17;
        case 18: return MR_r18;
        case 19: return MR_r19;
        case 20: return MR_r20;
        case 21: return MR_r21;
        case 22: return MR_r22;
        case 23: return MR_r23;
        case 24: return MR_r24;
        case 25: return MR_r25;
        case 26: return MR_r26;
        case 27: return MR_r27;
        case 28: return MR_r28;
        case 29: return MR_r29;
        case 30: return MR_r30;
        case 31: return MR_r31;
        case 32: return MR_r32;

    }

    /*NOTREACHED*/
    fprintf(stderr, "register %d out of range in get_reg\n", num);
    abort();
    return 0;
}

MR_Word
MR_set_reg(int num, MR_Word val)
{
    MR_restore_transient_registers();
    switch (num) {

        case 1:  MR_r1  = val; MR_save_transient_registers(); return val;
        case 2:  MR_r2  = val; MR_save_transient_registers(); return val;
        case 3:  MR_r3  = val; MR_save_transient_registers(); return val;
        case 4:  MR_r4  = val; MR_save_transient_registers(); return val;
        case 5:  MR_r5  = val; MR_save_transient_registers(); return val;
        case 6:  MR_r6  = val; MR_save_transient_registers(); return val;
        case 7:  MR_r7  = val; MR_save_transient_registers(); return val;
        case 8:  MR_r8  = val; MR_save_transient_registers(); return val;
        case 9:  MR_r9  = val; MR_save_transient_registers(); return val;
        case 10: MR_r10 = val; MR_save_transient_registers(); return val;
        case 11: MR_r11 = val; MR_save_transient_registers(); return val;
        case 12: MR_r12 = val; MR_save_transient_registers(); return val;
        case 13: MR_r13 = val; MR_save_transient_registers(); return val;
        case 14: MR_r14 = val; MR_save_transient_registers(); return val;
        case 15: MR_r15 = val; MR_save_transient_registers(); return val;
        case 16: MR_r16 = val; MR_save_transient_registers(); return val;
        case 17: MR_r17 = val; MR_save_transient_registers(); return val;
        case 18: MR_r18 = val; MR_save_transient_registers(); return val;
        case 19: MR_r19 = val; MR_save_transient_registers(); return val;
        case 20: MR_r20 = val; MR_save_transient_registers(); return val;
        case 21: MR_r21 = val; MR_save_transient_registers(); return val;
        case 22: MR_r22 = val; MR_save_transient_registers(); return val;
        case 23: MR_r23 = val; MR_save_transient_registers(); return val;
        case 24: MR_r24 = val; MR_save_transient_registers(); return val;
        case 25: MR_r25 = val; MR_save_transient_registers(); return val;
        case 26: MR_r26 = val; MR_save_transient_registers(); return val;
        case 27: MR_r27 = val; MR_save_transient_registers(); return val;
        case 28: MR_r28 = val; MR_save_transient_registers(); return val;
        case 29: MR_r29 = val; MR_save_transient_registers(); return val;
        case 30: MR_r30 = val; MR_save_transient_registers(); return val;
        case 31: MR_r31 = val; MR_save_transient_registers(); return val;
        case 32: MR_r32 = val; MR_save_transient_registers(); return val;

    }

    /*NOTREACHED*/
    fprintf(stderr, "register %d out of range in set_reg\n", num);
    abort();
    return 0;
}
