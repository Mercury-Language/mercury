define pr
set printregs("Register dump")
end

document pr
Prints the registers.
end

define pf
set dumpframe((Word *)mr9)
end

document pf
Prints the current nondet stack frame.
end

define ds
set dumpcpstack()
end

document ds
Dumps the whole nondet stack.
end

# Don't let abort actually run, as it will make stdio stop working.
b abort
