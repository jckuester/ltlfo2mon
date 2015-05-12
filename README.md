Ltlfo2mon
=========

Ltlfo2mon is an (automata-based) monitoring framework for a
first-order temporal logic, called LTLFO. It lets you reason about
arbitrary traces (i.e., sequences of events), where events are sets of
actions that can carry data; for example `{login('user1'),
logout('user2')},{logout('user2')}`.

The framework takes formulae specified in LTLFO as input (see
[Syntax](#syntax)) and generates monitors for them. The monitors then
verify whether a provided trace violates or satisfies the
specification. The optimised monitoring algorithm (based on
[Ltl3tools](http://ltl3tools.sourceforge.net/)) is part of the FMSD
Journal article [The ins and outs of first-order runtime
verification](http://dx.doi.org/doi:10.1007/s10703-015-0227-2),
whereas the earlier algorithm (based on
[lbt](http://www.tcs.hut.fi/Software/maria/tools/lbt/)) is presented
in the conference paper [From propositional to first-order
monitoring](http://kuester.multics.org/publications/RV13.pdf) at
[RV'13](http://rv2013.gforge.inria.fr/).

The master branch contains the current version of Ltlfo2mon, and the
folder `experiments/` the formulae, traces and monitor results for
FMSD. To see the version and the experiments for RV'13, please
checkout the branch
[rv2013](https://github.com/jckuester/ltlfo2mon/tree/rv2013).

Usage
-----

Ltlfo2mon can be used as a command line tool. Running `java -jar
ltlfo2mon.jar` will show you the following options:

```
ltlfo2mon v1.3
Usage: ltlfo2mon [options] <LTLFO formula> <trace>

  -o <file> | --output <file>
        Write monitor statistics (size, number of submonitors, etc.) to file.
  -p | --progression
        Use progression/formula rewriting as monitor.
  -sa | --sa-monitor
        Use deprecated SA-based monitor (default is optimized SA-based monitor, based on ltl3tools).
  -v | --verbose
        Show monitor's statistics (size, number of submonitors, etc.) after each step.
  -vv | --verbose level 2
        Show monitor's inner-state after each step.
  <LTLFO formula>
        LTLFO formula that gets monitored.
  <trace>
        Monitor reads single trace from stdin.
```

By default is used the optimised automata-based algorithm from the
FMSD article, but the parameters `--sa-monitor` or `--progression` let
you switch to the algorithm from RV'13 or progression/formula
rewriting. Note that you need to install
[Ltl3tools](http://ltl3tools.sourceforge.net/)) or
[lbt](http://www.tcs.hut.fi/Software/maria/tools/lbt/) to use the FMSD
or RV'13 algorithm respectively (see [Install](#install).


LTLFO formula syntax
-------

```
&phi; := ( &phi; ) | ~ &phi; | &phi; /\ &phi; | &phi; \/ &phi; | &phi; -> &phi; | &phi; <-> &phi; | G
&phi; | F &phi; | X &phi; | &phi; U &phi; | &phi; W &phi; | A (t1, ...,
tn):p. &phi; | E (x, ..., y):p. &phi; | true | false | p(t_1, ..., t_n) | r(t_1, ...., t_n)
```

`t_1, ..., t_n` are terms, i.e., variables, functions `f(t_1, ...,
t_n)` or constants. Functions take terms as input. Ltlfo2mon comes
with some predefined functions (e.g., `sum(x,y), sub(x,y), mul(x,y)`
and constants (e.g., `pi`). To read how to define your own, please
read [Configure](#configure).

The difference between interpreted predicates `p` (called I-Operator)
 and uninterpreted predicates `r` (called U-Operator) is that the
 former become true by its appearance in the trace, whereas the latter
 are evaluated by a program or algorithm (and cannot appear in the
 trace).  Therefore, I-Operators, like functions, require the
 definition of an evaluation algorithm (see
 [Configure](#configure)). The difference to functions is that the
 algorithm for I-Operators has to return `Boolean`. Ltlfo2mon comes
 with predefined I-Operators, such as ` eq(x,y), leq(x,y), even(x),
 odd(x)`.

 **Note:** U-Operators (like `v, w, login` in the following examples)
 don't have to be predefined (as they don't require an evaluation
 algorithm). The only requirement is that their names are not already
 reserved by I-ops, functions, variable names.

 **Also note:** The parser recognises string and integers automatically
 as constants, and defines them accordingly.

 All predicate, function, constant and variable names have to be Java
 identifiers.

Examples
-----

```
echo "{w(2)},{w(100),w(20)},{w(30)}" | java -jar ltlfo2mon.jar "G A x:w.even(x)"
```

This will return the result:

```
Result: ? after 3 events.
```

The parser recognises `1` as an integer constant automatically:

```
echo "{w(2),v(4)},{w(12),v(7)},{}" | java -jar ltlfo2mon.jar "G A x:w. E y:v.leq(add(x,1),y)"
```

This will return the result:

```
Result: ë°Õ after 2 events.
```

The parser recognises `'user\d*'` as a string constant automatically:

```
echo "{login('user1')},{login('user5')},{}" | java -jar ltlfo2mon.jar "G A x:login. regex(x,'user\d*')"
```

Configure
---------

Custom predicates, functions or constants can be defined in
`src/main/scala/ltlfo2mon/Conf.scala`. For example, the following line
adds the definition of the interpreted predicate `even(x)`:

```
struct.addIoperator("even", (args: Vector[Any]) => args(0).toString.toInt % 2 == 0)
```

Here, `even` is the name of the predicate, and `(args: Vector[Any]) =>
args(0).toString.toInt % 2 == 0` is the algorithm to evaluate the
truth value of the predicate. It is defined as a function of signature
`Vector[Any] => Boolean` that takes a vector of arguments as input and
returns true or false. Note, that predicates and functions are not
type-safe: you have to assure that `even(x)` takes a unary predicate
`x` as input, which has to be an integer, otherwise the monitor will
crash or misbehave at runtime. Functions can be defined similar to
predicates with `addFunct(name: String, interpr: Vector[Any] => Any)`.

To define a constant with name `pi` and value `3.14` add:

```
struct.addConst("pi", 3.14)
```

To recompile `ltlfo2mon.jar` after making changes run `sbt` (requires
sbt 0.13.x) in the base-directory of this project and then write
`assembly` and press `RETURN`:

```
$ sbt
[info] Loading project definition from ./src/scala/ltlfo2mon/project
[info] Set current project to ltlfo2mon (in build file:./src/scala/ltlfo2mon/)
> assembly
[info] Packaging ./target/scala-2.10/ltlfo2mon.jar ...
[info] Done packaging.
[success] Total time: 4 s, completed Jul 16, 2013 2:56:14 PM
```

The jar is written to `/target/scala-2.10/`.

Installing
----------

For the optimised algorithm (default) you need to install
[Ltl3tools](http://ltl3tools.sourceforge.net/). Then edit the
installation directory in `ltl3tools.sh` and add the script to your
`PATH`.

For the RV'13 algorithm (argument `--sa-monitor`) you first need to
install [lbt](http://www.tcs.hut.fi/Software/maria/tools/lbt/), which
converts a linear temporal logic formula to a generalised BÅ¸chi
automaton. For Debian/Ubuntu you can install it via apt:

```
apt-get install lbt
```

**Note:** You don't need to install any third-party tool if you run
Ltlfo2mon with progression/formula rewriting (argument `-p`).

License
-------

GPLv3 License.
