#!/bin/bash

# create files with look-up-table size
sed -n -e '/Size-lookup-table:/ s/.*\: *//p' > $1/lookuptables_sa.dat $1/*sa.dat
sed -n -e '/Size-lookup-table:/ s/.*\: *//p' > $1/lookuptables_opt.dat $1/*optimised.dat

