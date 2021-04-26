#!/usr/bin/awk -f
# vim: ft=awk ts=4 sw=4 et
BEGIN   {
            NUM_FIELDS = 100;
            for (i = 0; i < NUM_FIELDS ; i++)
            {
                read[i] = 0;
                same[i] = 0;
                diff[i] = 0;
            }
        }
$1 == "stat_rsd" {
            read[$2] += $3;
            same[$2] += $4;
            diff[$2] += $5;
        }
END     {
            printf("%2s %9s %9s %9s %7s\n",
                "i", "read", "same", "diff", "same%");
            for (i = 0; i < NUM_FIELDS; i++)
            {
                if (read[i] != 0 || same[i] != 0 || diff[i] != 0)
                {
                    printf("%2d %9d %9d %9d", i, read[i], same[i], diff[i]);
                    if (0 + same[i] + diff[i] > 0)
                    {
                        printf(" %6.2f%%\n",
                            (100.0 * same[i]) / (same[i] + diff[i]));
                    }
                    else
                    {
                        printf("\n");
                    }
                }
            }
        }
