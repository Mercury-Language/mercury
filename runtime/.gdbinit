#-----------------------------------------------------------------------------#

# .gdbinit.arch should be a link to .gdbinit.sparc
# source .gdbinit.arch


# file sys
# break main
# break call_engine
# run -wappend_1

#-----------------------------------------------------------------------------#



define pr
gdb-save-saved-regs	# save saved_regs[] array
gdb-get-regs		# copy registers to gdb vars $mr0, $mr1, ...
			# (using .gdbinit.arch)
gdb-save-regs		# copy registers $mr0, ... to saved_regs[] array
set printregs("Register dump")
gdb-restore-saved-regs	# restore saved_regs[] array
end
document pr
Prints the registers.
Doesn't work if you use -DUSE_GCC_GLOBAL_REGISTERS,
unless you also `source .gdbinit.<machine>'.
end
#-----------------------------------------------------------------------------#
define pr1
set printregs("Register dump")
end
document pr1
Prints the registers
end
#-----------------------------------------------------------------------------#
define pr2
gdb-get-regs
printf "succip = %d (0x%x)\n", $mr0, $mr0
printf "r1     = %d (0x%x)\n", $mr1, $mr1
printf "r2     = %d (0x%x)\n", $mr2, $mr2
printf "r3     = %d (0x%x)\n", $mr3, $mr3
printf "r4     = %d (0x%x)\n", $mr4, $mr4
end
document pr2
Prints some of the registers
Works using only gcc, doesn't call any C functions.
end
#-----------------------------------------------------------------------------#
define pmf
gdb-get-regs
set dumpframe($mr8)
end
document pmf
Prints the maximum (top) nondet stack frame.
end
#-----------------------------------------------------------------------------#
define pf
gdb-get-regs
set dumpframe($mr8)
end
document pf
Prints the current nondet stack frame.
end
#-----------------------------------------------------------------------------#
define ps
set dumpcpstack()
end
document ps
Prints the whole nondet stack.
end
#-----------------------------------------------------------------------------#

define gdb-save-saved-regs
set $tmp0 = saved_regs[0]
set $tmp1 = saved_regs[1]
set $tmp2 = saved_regs[2]
set $tmp3 = saved_regs[3]
set $tmp4 = saved_regs[4]
set $tmp5 = saved_regs[5]
set $tmp6 = saved_regs[6]
set $tmp7 = saved_regs[7]
set $tmp8 = saved_regs[8]
set $tmp9 = saved_regs[9]
end

define gdb-restore-saved-regs
set saved_regs[0] = $tmp0
set saved_regs[1] = $tmp1
set saved_regs[2] = $tmp2
set saved_regs[3] = $tmp3
set saved_regs[4] = $tmp4
set saved_regs[5] = $tmp5
set saved_regs[6] = $tmp6
set saved_regs[7] = $tmp7
set saved_regs[8] = $tmp8
set saved_regs[9] = $tmp9
end

define gdb-save-regs
set saved_regs[0] = $mr0
set saved_regs[1] = $mr1
set saved_regs[2] = $mr2
set saved_regs[3] = $mr3
set saved_regs[4] = $mr4
set saved_regs[5] = $mr5
set saved_regs[6] = $mr6
set saved_regs[7] = $mr7
set saved_regs[8] = $mr8
set saved_regs[9] = $mr9
end
