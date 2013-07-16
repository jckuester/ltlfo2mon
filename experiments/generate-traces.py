#!/usr/bin/python

import math
import random

trace_length=10000
num_of_traces=1
max_event_size=10
iOp="w"
mu=2
sigma=0.5

def generate_action(iOp_name, mu, sigma):
	value = int(math.ceil(random.lognormvariate(mu,sigma)))
	return iOp_name+"("+str(value)+")"

def generate_event(max_event_size):
	event=""
	for i in range(0,random.randint(0,max_event_size)):
		if i==0:
			event += generate_action(iOp,mu,sigma)
		else:
			event += "," + generate_action(iOp,mu,sigma)
	return "{" + event + "}"


def generate_trace(trace_length):
        trace=""
        for i in range(0,trace_length):
                if i==0:
                        trace += generate_event(max_event_size)
                else:
                        trace += "," + generate_event(max_event_size)
        return trace


file=open("traces_"+str(trace_length)+".dat","a")
for i in range(0,num_of_traces):
	trace = generate_trace(trace_length)
	print trace
	file.write(trace + "\n")
file.close()

	
