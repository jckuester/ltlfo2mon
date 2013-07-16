Ltlfo2mon for RV'13
===================

Ltlfo2mon is an automata-based monitoring framework for a first-order temporal logic, called LTLFO. It generates and executes monitors for policies specified in LTLFO to verify if a provided trace of events violates or satisfies them. This work is based on the conference paper [**From propositional to first-order monitoring**](http://kuester.multics.org/publications/RV13.pdf)"" and will be presented at [RV'13](http://rv2013.gforge.inria.fr/).

Usage
-----

It can be used as a command line tool. If you run `java -jar ltlfo2mon.jar` it will show you the following options:

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

It comes with predefined interpreted predicates `eq(x,y), leq(x,y), even(x), odd(x)`, and the uninterpreted predicate `w` to specify actions in the trace. To define your own predicates, functions or constants please read #Configure

For example run:

```
java -jar ltlfo2mon.jar "G A x:w.even(x)" "{w(2)},{w(100),w(20)},{w(30)}"
```

This will return the result:

```
Result: ? after 3 events.
```

```
 java -jar ltlfo2mon.jar "G A x:w. E y:q.leq(x,y)" "{w(2),w(4)},{},{w(3),w(3)}"

```

```
Result: ⊥ after 3 events.
```

Configure
---------

To define custom predicates, functions or constants you have to edit `src/main/scala/ltlfo2mon/Conf.scala` and add them to the existing first-order structure `struct`. For example, the following line adds the definition of the uninterpreted predicate `w`:

```
struct.addUoperator("w") 
```

Furthermore, the interpreted predicate `even` is created as:

```
struct.addIoperator("even", (args: Vector[Any]) => args(0).toString.toInt % 2 == 0)
```

Here, "even" is the name of the predicate, and `(args: Vector[Any]) => args(0).toString.toInt % 2 == 0` is the algorithm to evaluate the truth value of the predicate. It is defined as a function of type `Vector[Any] => Boolean` that takes a vector of arguments as input and returns true or false. Note, that predicates and functions are not type-safe yet: you have to know that `even(x)` is a unary predicate and `x` has to be an integer. Functions can be defined similar to predicates with `addFunct(name: String, interpr: Vector[Any] => Any)`.

To define a constant with name `3` and value `3` add

```
struct.addConst("3", 3)
```

You can recompile `ltlfo2mon.jar` after making changes by running `sbt` in the base-directory of this project and then type `assembly` and press `RETURN`:

```
$ sbt
[info] Loading project definition from ./src/scala/ltlfo2mon/project
[info] Set current project to ltlfo2mon (in build file:./src/scala/ltlfo2mon/)
> assembly
[info] Packaging /home/jkuster/phd/src/scala/ltlfo2mon/target/scala-2.10/ltlfo2mon.jar ...
[info] Done packaging.
[success] Total time: 4 s, completed Jul 16, 2013 2:56:14 PM
```

Experiments
----------

In the folder `experiments/` you find the scripts and data, which have been used to generate the experiments for `Fig. 2` in the RV'13 conference paper. The python script `generate-traces.py` generates you test-traces (you can define its length, the maximal size of events, and the parameters of a lognormal derivation for values of predicate `w`). The bash-script `run-experiments.sh` runs the experiment for formulae in `formulae.dat` and traces in `traces.dat`. You can find the results from the conference in `results/`. Finally, `experiments-tikz.r` is an R-script that calculates the statistics and plots the diagram.

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
