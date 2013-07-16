Ltlfo2mon for RV'13
===================

Ltlfo2mon is an automata-based monitoring framework for a first-order temporal logic, called LTLFO. It generates and executes monitors for policies specified in LTLFO to verify if a provided trace of events violates or satisfies them. This work is part of the conference paper [From propositional to first-order monitoring](http://kuester.multics.org/publications/RV13.pdf) and will be presented at [RV'13](http://rv2013.gforge.inria.fr/).

Usage
-----

It can be used as a command line tool. If you run `java -jar ltlfo2mon.jar` it will show you the following options:

```
ltlfo2mon v1.0 beta
Usage: ltlfo2mon [options] <ltlfo-formula> <trace-on-stdin>

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
  <trace-on-stdin>
        Monitor reads a single trace from stdin.
```

It comes with predefined interpreted predicates `eq(x,y), leq(x,y), even(x), odd(x)`, and the uninterpreted predicates `v, w` to specify actions in the trace. To define your own predicates, functions or constants, please read [Configure](#configure).

For example run:

```
echo "{w(2)},{w(100),w(20)},{w(30)}" | java -jar ltlfo2mon.jar "G A x:w.even(x)"
```

This will return the result:

```
Result: ? after 3 events.
```

Or run:

```
echo "{w(2),v(4)},{},{w(3),v(3)}" | java -jar ltlfo2mon.jar "G A x:w. E y:v.leq(x,y)"
```

This will return the result:

```
Result: ⊥ after 3 events.
```

Configure
---------

Custom predicates, functions or constants can be defined in `src/main/scala/ltlfo2mon/Conf.scala`. For example, the following line adds the definition of the uninterpreted predicate `w` to the monitor:

```
struct.addUoperator("w") 
```

Furthermore, the interpreted predicate `even(x)` can be defined with:

```
struct.addIoperator("even", (args: Vector[Any]) => args(0).toString.toInt % 2 == 0)
```

Here, `even` is the name of the predicate, and `(args: Vector[Any]) => args(0).toString.toInt % 2 == 0` is the algorithm to evaluate the truth value of the predicate. It is defined as a function of signature `Vector[Any] => Boolean` that takes a vector of arguments as input and returns true or false. Note, that predicates and functions are not type-safe: you have to assure that `even(x)` takes a unary predicate `x` as input, which has to be an integer, otherwise the monitor will crash or misbehave at runtime. Functions can be defined similar to predicates with `addFunct(name: String, interpr: Vector[Any] => Any)`.

To define a constant with name `3` and value `3` add:

```
struct.addConst("3", 3)
```

You can recompile `ltlfo2mon.jar` after making changes by running `sbt` in the base-directory of this project and then write `assembly` and press `RETURN`:

```
$ sbt
[info] Loading project definition from ./src/scala/ltlfo2mon/project
[info] Set current project to ltlfo2mon (in build file:./src/scala/ltlfo2mon/)
> assembly
[info] Packaging ./target/scala-2.10/ltlfo2mon.jar ...
[info] Done packaging.
[success] Total time: 4 s, completed Jul 16, 2013 2:56:14 PM
```

Experiments
----------

In the folder `experiments/` you find the scripts and data, which have been used to generate the experiments for `Fig. 2` in [From propositional to first-order monitoring](http://kuester.multics.org/publications/RV13.pdf):

- The python script `generate-traces.py` generates you test traces (you can define its length, the maximal size of events, and parameters of a log-normal distribution for values of predicate `w`).
- The bash-script `run-experiments.sh` runs the monitor for formulae in `formulae.dat` and traces in `traces_10000.dat`. You can find the results of the experiments for the conference in `results/`.
- `experiments-tikz.r` is an R-script that calculates the statistics and plots the diagram.

Installing
----------

You first need to install lbt (http://www.tcs.hut.fi/Software/maria/tools/lbt/), that converts a linear temporal logic formula to a generalised Büchi automaton. For Debian/Ubuntu you can install it via apt:

```
apt-get install lbt
```

Then, either download the standalone ltlfo2mon.jar in the base directory, or run and build the project with sbt 0.12.x. 

License
-------

GPLv3 License.
