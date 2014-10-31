#!/bin/bash

cp ../target/scala-2.10/ltlfo2mon.jar .
export PATH=~/bin:~/bin/lbt-1.2.2:$PATH

trace_length=( 10000 )

declare -i i=1
while read formula
do
        declare -i j=1
        for t_len in "${trace_length[@]}"
        do
                while read trace
                do
                        echo "$trace" | java -jar ./ltlfo2mon.jar "$formula" -sa -o results/formula_"$i"_traces_"$t_len"_"$j"_sa.dat&
                        echo "$trace" | java -jar ./ltlfo2mon.jar "$formula" -p -o results/formula_"$i"_traces_"$t_len"_"$j"_progression.dat&
 			echo "$trace" | java -jar ./ltlfo2mon.jar "$formula" -o results/formula_"$i"_traces_"$t_len"_"$j"_optimised.dat
                j+=1
                done < traces_"$t_len".dat
        done
i+=1
done < formulae.dat

