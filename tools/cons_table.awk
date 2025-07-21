#!/usr/bin/awk -f
# vim: ft=awk ts=4 sw=4 et
$2 == "DU_CTORS" {
            name_count++;
            name_sum += $NF;
            if ($NF > name_max) {
                name_max = $NF;
            }
        }
$2 == "FQ_DU_CTOR" {
            ctor_count++;
            ctor_sum += $NF;
            if ($NF > ctor_max) {
                ctor_max = $NF;
            }
        }
END     {
            printf("#names = %8d, #ctors =    %8d, #ctors/name =    %6.2f, max = %2d\n",
                name_count, name_count, (name_sum/name_count), name_max);
            printf("#ctors = %8d, #synonyms = %8d, #synonyms/ctor = %6.2f, max = %2d\n",
                ctor_count, ctor_count, (ctor_sum/ctor_count), ctor_max);
        }

