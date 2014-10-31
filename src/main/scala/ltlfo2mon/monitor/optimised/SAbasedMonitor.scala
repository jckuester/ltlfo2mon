/** *****************************************************************************
  * This is part of ltlfo2mon (https://github.com/jckuester/ltlfo2mon).
  *
  * Copyright (c) 2013 by Jan-Christoph Kuester <kuester@sdf.org>
  *
  * Ltlfo2mon is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * Ltlfo2mon is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ltlfo2mon.  If not, see <http://www.gnu.org/licenses/>.
  * *****************************************************************************/
package ltlfo2mon.monitor.optimised

import java.io._

import ltlfo2mon.Conf
import ltlfo2mon.datatype.Types._
import ltlfo2mon.datatype._
import ltlfo2mon.monitor.IMonitor
import ltlfo2mon.util.Printable._
import ltlfo2mon.util.parsing.LTL3ToolsParser

import scala.collection._
import scala.collection.mutable.ListBuffer
import scala.util.Marshal
import scala.util.control.Breaks._


class SAbasedMonitor(phi: Formula, struct: Structure, v: Valuation = immutable.HashMap[String, Any](),
                     var cachedBFormulae: mutable.HashSet[BooleanFormula] = mutable.HashSet[BooleanFormula](),
                     isRootMonitor: Boolean = true) extends IMonitor(phi) with BooleanFormula {

  val name = "opt. SA-based monitor"

  override def toString = "M: " + phi + {
    if (v.isEmpty) "" else v.mkString("{", ", ", "}")
  }

  // the LTL3 moor machine that was created before this monitor runs
  lazy val states: States = SAbasedMonitor.lookupTable(phi)

  // because this monitor is kind of non-deterministic we need to store a run tree
  var runTreeRoot: RunState = RunState(states.initialState)

  // caches a reference to the leaves in runTree
  var runTreeLeaves: Option[ListBuffer[RunState]] = None

  /*
   * M2 & M3: forward event to SA_phi and SA_notphi, and communicate verdict.
   */
  def process(event: Event): Boolean3 = {
    if (result.isEmpty || isRootMonitor) {

      val createdForallConjs: mutable.HashMap[Forall, Conj] = new mutable.HashMap[Forall, Conj]()

      /*
     * T3: extend runs.
     */
      runTreeLeaves match {
        case None =>
          // no runs yet, monitor is in initial phase
          for (t <- delta(states.initialState, event)) {
            runTreeRoot.runTransitions += RunTransition(spawn(t.guard, event, createdForallConjs), RunState(t.targetState))
          }
        case Some(exRunTreeLeaves) =>
          for (leaf <- exRunTreeLeaves) {
            leaf.state.ltl3type match {
              case Top =>
              case Bottom =>
              case Unknown =>
                for (t <- delta(leaf.state, event)) {
                  leaf.runTransitions += RunTransition(spawn(t.guard, event, createdForallConjs), new RunState(t.targetState))
                }
            }
          }
      }

      runTreeLeaves = Some(runTreeRoot.process(event, cachedBFormulae))

      if (isRootMonitor) {
        val tmpCachedBFormulae = cachedBFormulae.filter(_.result match { case None => false case Some(ltl3type) => ltl3type == Unknown})
        cachedBFormulae.clear()
        cachedBFormulae ++= tmpCachedBFormulae
        // reset results
        cachedBFormulae.foreach(_.result = None)
      }

      /*
       * M3/T10: return if buffer has only runs ending in "top" states (resp. "bottom" states) or some ending in "?" states
       */
      if (runTreeLeaves.get.forall(_.state.ltl3type == Top)) {
        result = Some(Top)
        Top
      } else if (runTreeLeaves.get.forall(_.state.ltl3type == Bottom)) {
        result = Some(Bottom)
        Bottom
      } else {
        result = Some(Unknown)
        Unknown
      }
    } else {
      // get result from cache (already executed)
      result.get
    }
  }

  /*
   * processes a trace
   */
  def process(trace: Trace, outputFile: String): (Boolean3, Int) = {

    var result: Boolean3 = null
    var sizeOfEvents: List[Int] = List()
    var size: List[Int] = List()
    var numOfSubmonitors: List[Int] = List()
    var maxNumOfRuns: List[Int] = List()
    var maxNumOfObls: List[Int] = List()
    var maxOblLength: List[Int] = List()

    /*
     * experiments file output header
     */
    var printWriter: Option[PrintWriter] = None
    if (outputFile != "") {
      try {
        printWriter = Some(new PrintWriter(new FileOutputStream(new File(outputFile), true)))
      } catch {
        case e: FileNotFoundException => println(e.getMessage); sys.exit()
      }
    }

    printWriter match {
      case None =>
      case Some(writer) =>
        writer.println("# " + this.name)
        writer.println("# Formula: " + this.phi)
        //writer.println("#Trace: " + trace)
        writer.println("# Trace-length: " + trace.length)
        writer.println("# Size-lookup-table: " + SAbasedMonitor.sizeLookupTable)
        writer.println("#")
        writer.println("# Ev-sz = Event-size, M-sz = Mon-size, Exec-ms = Execution time in ms, #mons = Total num mons")
        writer.println("# M#runs / M#rs = max number of runs, MLruns / MLrs = max length runs, AV#runs / AV#rs = avg number of runs, AVLruns / AVLrs = avg length of runs")
        writer.println("#")
        writer.print("#Time" + "\t" + "Ev-sz" + "\t" + "M-sz" + "\t" + "Exec-ms" + "\t" + "#mons" +
          "\t" + "M#runs" + "\t" + "MLruns" + "\t" + "MLobl" +
          "\t" + "AV#runs" + "\t" + "AVLruns" + "\t" + "AVLobl")
        for (level <- 0 to this.level) {
          writer.print("\t" + "L" + level + "#mons" +
            "\t" + "L" + level + "M#rs" + "\t" + "L" + level + "MLrs" + "\t" + "L" + level + "MLobl" +
            "\t" + "L" + level + "AV#rs" + "\t" + "L" + level + "AVLrs" + "\t" + "L" + level + "AVLo")
        }
        writer.println()
    }

    breakable {
      for ((event, i) <- trace.events.zipWithIndex) {
        val index = i + 1
        Conf.index = index

        /*
         * process event
         */
        var start, end: Long = 0
        start = System.currentTimeMillis()
        result = this.process(event)
        end = System.currentTimeMillis()

        val monSize = this.size

        sizeOfEvents = event.size :: sizeOfEvents
        size = monSize :: size
        numOfSubmonitors = this.numOfMonitors() :: numOfSubmonitors
        maxNumOfRuns = this.maxNumOfRuns() :: maxNumOfRuns
        maxNumOfObls = this.maxNumOfObls() :: maxNumOfObls
        maxOblLength = this.maxOblLength() :: maxOblLength

        /*
         * experiments file output
         */
        printWriter match {
          case None =>
          case Some(writer) =>
            writer.print(index + "\t" + event.size + "\t" + monSize + "\t" + (end - start) +
              "\t" + this.numOfMonitors() + "\t" + this.maxNumOfRuns() + "\t" + this.maxNumOfObls() + "\t" + this.maxOblLength() + 
              "\t" + this.avgNumOfRuns() + "\t" + this.avgNumOfObls() + "\t" + this.avgOblLength())
            for (level <- 0 to this.level) {
              writer.print("\t" + this.numOfMonitors(level) +
                "\t" + this.maxNumOfRuns(level) + "\t" + this.maxNumOfObls(level) + "\t" + this.maxOblLength(level) +
                "\t" + this.avgNumOfRuns(level) + "\t" + this.avgNumOfObls(level) + "\t" + this.avgOblLength(level))
            }
            writer.println()
        }

        if (Conf.verbose2) {
          println("\n### TIME " + index + " ###")
          println("Monitor size: " + monSize)
          println("Number of monitors: " + this.numOfMonitors())
          println("Max number of runs: " + this.maxNumOfRuns())
          println("Max length of runs: " + this.maxNumOfObls())
          println("Max length of obligation: " + this.maxOblLength())
          println("Event size: " + event.size)

          /*
           * print inner monitor state
           */
          for (level <- this.level to 0 by -1) {
            val indent = this.level - level + 1
            val levelMons = this.getMonitors(level)
            println("\n" + "\t" * indent + "L" + level + " Monitors: " + levelMons.size)
            println("\t" * indent + "###")
            levelMons.foreach(m => println("\t" * indent + m
              + "\n" + "\t" * (indent + 1) + m.runTreeRoot.toString(indent)))
          }
        }


        result match {
          case Top | Bottom => break()
          case Unknown =>
        }
      }
    }

    /*
     * print final results
     */
    if (Conf.verbose) {
      if (Conf.verbose2) println("\n\n### FINAL RESULT ###")
      else println("### FINAL RESULT ###")
      println(this.name)
      println("Formula: " + this.phi)
      println("Result: " + result + " after " + size.length + " events.")
      println("Trace length: " + trace.length + "\n")

      val maxDigits = size.reverse.map(e => e.toString.length).max

      println("Monitor size:\t\t\t\t" + size.reverse.map(e => {
        " " * (maxDigits - e.toString.length)
      } + e).mkString(",\t"))
      println("Number of monitors:\t\t\t" + numOfSubmonitors.reverse.map(e => {
        " " * (maxDigits - e.toString.length)
      } + e).mkString(",\t"))
      println("Max number of runs:\t\t\t" + maxNumOfRuns.reverse.map(e => {
        " " * (maxDigits - e.toString.length)
      } + e).mkString(",\t"))
      println("Max length of runs:\t\t\t" + maxNumOfObls.reverse.map(e => {
        " " * (maxDigits - e.toString.length)
      } + e).mkString(",\t"))
      println("Max length of obligations:\t" + maxOblLength.reverse.map(e => {
        " " * (maxDigits - e.toString.length)
      } + e).mkString(",\t"))
      println("Size of events:\t\t\t\t" + sizeOfEvents.reverse.map(e => {
        " " * (maxDigits - e.toString.length)
      } + e).mkString(",\t"))
    }
    printWriter match {
      case None =>
      case Some(writer) =>
        writer.println()
        writer.close()
    }

    (result, size.length)
  }

  private def delta(q: State, event: Event): immutable.Set[Transition] = {
    q.transitions.filter(t => t.interpret(struct, event, v))
  }

  private def spawn(formulae: Formulae, event: Event, createdForallConjs: mutable.HashMap[Forall, Conj]): Conj = {
    /*
     * Spawning conjunction: conjunction that contains universal-conjunctions and exists-disjunctions.
     */
    val bf = new Conj(cachedBFormulae)

    formulae.foreach {
      case forall: Forall =>
        helperSpawn(bf, event, createdForallConjs, forall, neg = false)
      // Exists-Disjunction
      case Not(psi) =>
        psi match {
          case forall: Forall =>
            helperSpawn(bf, event, createdForallConjs, forall, neg = true)
          case _ =>
        }
      case _ =>
    }

    bf
  }

  private def helperSpawn(bf: Conj, event: Event, createdForallConjs: mutable.HashMap[Forall, Conj], forall: Forall, neg: Boolean): Unit = {
    // check if cached forall-conjunction exists
    createdForallConjs.get(forall) match {
      case None =>
        var conj = new Conj(cachedBFormulae)
        // for all actions (p,d) of event
        for (action <- event if action._1 == forall.p.name) {
          // create new valuation
          var vNew: Valuation = v
          for ((variable, i) <- forall.p.args.zipWithIndex) {
            vNew += ((variable.name, action._2(i)))
          }
          // create sub-monitor
          conj.elems += new SAbasedMonitor(forall.phi, struct, vNew, cachedBFormulae, false)
        }
        // store forall-conjunction in cache
        createdForallConjs.put(forall, conj)
        // add forall-conjunction to spawning-conjunction
        if (neg) {
          bf.elems += new Neg(cachedBFormulae, conj)
        } else {
          bf.elems += conj
        }
      // get existing forall-conjunction from cache
      case Some(exConj) =>
        if (neg) {
          bf.elems += new Neg(cachedBFormulae, exConj)
        } else {
          bf.elems += exConj
        }
    }
  }

  /*
   * log stuff
   */
  def level = this.phi.level

  def length = 1

  def size = monSize()

  def monSize(bfHashSet: mutable.HashSet[BooleanFormula] = new mutable.HashSet[BooleanFormula]()): Int = {
    if (bfHashSet(this)) {
      0
    } else {
      bfHashSet.add(this)
      runTreeRoot.monSize(bfHashSet)
    }
  }

  def numOfMonitors(level: Int = -1): Int = getMonitors(level).size

  // maximal # of runs per monitor
  def maxNumOfRuns(level: Int = -1) = getMonitors(level).map(m => m.runTreeLeaves.getOrElse(Set()).size).reduceOption(_ max _).getOrElse(0)

  // average # of runs per monitor
  def avgNumOfRuns(level: Int = -1) = getMonitors(level).map(m => m.runTreeLeaves.getOrElse(Set()).size).avg

  // longest run
  def maxNumOfObls(level: Int = -1) = getMonitors(level).toSeq.map(m => m.runTreeRoot.lengthOfRuns).flatten.reduceOption(_ max _).getOrElse(0)

  // average length of runs
  def avgNumOfObls(level: Int = -1) = getMonitors(level).toSeq.map(m => m.runTreeRoot.lengthOfRuns).flatten.avg

  // longest obligation (boolean formula)
  def maxOblLength(level: Int = -1) = getMonitors(level).toSeq.map(_.cachedBFormulae.toSeq.map(_.length)).flatten.reduceOption(_ max _).getOrElse(0)

  // average length of obligation (boolean formula)
  def avgOblLength(level: Int = -1) = getMonitors(level).toSeq.map(_.cachedBFormulae.toSeq.map(_.length)).flatten.avg

  def getMonitors: Set[SAbasedMonitor] = {
    if (isRootMonitor) {
      cachedBFormulae.map(obl => obl.getMonitors).asInstanceOf[Set[Set[SAbasedMonitor]]].flatten + this
    } else {
      Set(this)
    }
  }

  def getMonitors(level: Int = -1): Set[SAbasedMonitor] = if (level == -1) this.getMonitors else this.getMonitors.filter(_.level == level)

  override def equals(that: Any): Boolean = {
    that.isInstanceOf[SAbasedMonitor] && (this.hashCode() == that.asInstanceOf[SAbasedMonitor].hashCode())
  }

  override def hashCode = phi.hashCode() + v.hashCode() + runTreeRoot.hashCode()
}

object SAbasedMonitor {
  /*
   * global variable, pre-constructed SAs are loaded at runtime
   */
  var lookupTable: mutable.HashMap[Formula, States] = null

  def setLookupTable(policy: Formula) = {
    lookupTable = mutable.HashMap[Formula, States]()
    policy.getPhiOfQuantifiedSFs.foreach { f => SAbasedMonitor.lookupTable.put(f, new LTL3ToolsParser(f).parse())}
    println(policy.getPhiOfQuantifiedSFs)
  }

  def storeLookupTableToFile(file: String) = {
    try {
      val out = new FileOutputStream(file)
      out.write(Marshal.dump(lookupTable))
      out.close()
      println("INFO: Look-up table written to file " + file + ".")
    } catch {
      case e: FileNotFoundException => println(e.getMessage); sys.exit()
    }
  }

  def setLookupTableFromFile(file: String) = {
    try {
      val in = new FileInputStream(file)
      val bytes = Stream.continually(in.read).takeWhile(-1 !=).map(_.toByte).toArray
      lookupTable = Marshal.load[mutable.HashMap[Formula, States]](bytes)
    } catch {
      case e: FileNotFoundException => println(e.getMessage); sys.exit()
    }
  }

  def sizeLookupTable = lookupTable.values.toSeq.map(states => states.size).sum
}