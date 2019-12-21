set history filename ~/.gdb_history
set history save on
set print object
set print vtbl

define print_vector
if $argc == 2
set $elem = $arg0.size()
if $arg1 >= $arg0.size()
printf "Error, %s.size() = %d, printing last element:\n", "$arg0", $arg0.size()
set $elem = $arg1 -1
end
print *($arg0._M_impl._M_start + $elem)@1
else
print *($arg0._M_impl._M_start)@$arg0.size()
end
end
