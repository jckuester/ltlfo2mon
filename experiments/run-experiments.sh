#!/bin/bash

trace_length=( 10000 )

declare -i i=0
while read formula
do
	for t_len in "${trace_length[@]}"
	do
		while read trace
		do
			echo "$trace" | java -jar ../ltlfo2mon.jar "$formula" -o results/formula_"$i"_traces_"$t_len"_sa.dat
			echo "$trace" | java -jar ../ltlfo2mon.jar "$formula" -p -o results/formula_"$i"_traces_"$t_len"_progression.dat
		done < traces_"$t_len".dat
	done
i+=1
done < formulae.dat

