Ltlfo2mon
=========

Ltlfo2mon is an automata-based monitoring framework for a first-order temporal logic, called LTLFO. It can generate and execute monitors for policies specified in LTLFO to verify if a provided trace of events violates or satisfies them. (for more information read the technical report "From propositional to first-order monitoring" on either http://kuester.multics.org/ or http://baueran.multics.org/) 

Usage
-----

It is used as a command line tool. If you run `java -jar ltlfo2mon.jar` it will show you the following options:

```
ltlfo2mon v1.0 beta
Usage: ltlfo2mon [options] <ltlfo-formula> <trace>

  -o <file> | --output <file>
        Write montior's statistics (size, number of submonitors, etc.) to file.
  -p | --progression
        Use progression/formula-rewriting as monitor; default is SA-based monitor.
  -v | --verbose
        Show monitor's statistics (size, number of submonitors, etc.) after each step.
  -vv | --verbose level 2
        Show monitor's inner-state after each step.
  <ltlfo-formula>
        LTLFO formula.
  <trace>
        Trace.
```

In this beta-version, you can only use the predefined interpreted predicates `eq(x,y), leq(x,y), even(x), odd(x)`, and the uninterpreted predicates `p, q` to specify actions in the trace.

For example run:

```
java -jar ltlfo2mon.jar "G A x:p.even(x)" "{p(2)},{p(100),p(20)},{p(30)}"
```

This will return the result:

```
Result: ? after 3 events.
```

Or run:

```
java -jar ltlfo2mon.jar "G A x:p.E y:q.leq(x,y)" "{p(2),q(4)},{},{p(3),q(3)}"
```

This will return the result:

```
Result: ⊥ after 3 events.
```

Installing
----------

You first need to install `lbt` (http://www.tcs.hut.fi/Software/maria/tools/lbt/), a tool that converts a linear temporal logic formula to a generalised Büchi automaton. For Debian/Ubuntu you can install it via `apt`:

```
apt-get install lbt
```

Then, either download the standalone `ltlfo2mon.jar` in the base directory, or run and build the project with `sbt 0.12.x`. 

License
-------

GPLv3 License.
