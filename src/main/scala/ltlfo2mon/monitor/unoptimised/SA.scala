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
package ltlfo2mon.monitor.unoptimised

import java.io.{FileInputStream, FileNotFoundException, FileOutputStream}

import ltlfo2mon.datatype.Types._
import ltlfo2mon.datatype._
import ltlfo2mon.util.Printable._
import ltlfo2mon.util.parsing.LBTParser

import scala.collection._
import scala.util.Marshal

/*
 * Algorithm T
 */
class SA(phi: Formula, struct: Structure, v: Valuation) {
  type Runs = mutable.Buffer[Run]

  override def toString = "T: " + phi + {
    if (v.isEmpty) "" else v.mkString("{", ", ", "}")
  }

  val states: Set[State] = SA.lookupTable(phi)
  var buffer: Option[mutable.Buffer[Run]] = None

  /*
   * read next event.
   */
  def process(event: Event, createdConjs: mutable.HashMap[String, Conj], executedSubmons: mutable.HashMap[Int,Boolean3]): Boolean = {
    /*
     * T3: extend runs.
     */
    buffer match {
      case None =>
        var newBuffer: Runs = mutable.Buffer()
        for (initState <- states.filter(_.isInitial)) {
          delta(initState, event) match {
            case None =>
            case Some(targetStates) =>
              for (targetState <- targetStates)
                newBuffer += Run(targetState, mutable.Buffer(spawn(initState, event, createdConjs)))
          }
        }
        buffer = Some(newBuffer)
      case Some(runs) =>
        var newBuffer: Runs = mutable.Buffer()
        for (run <- runs) {
          delta(run.state, event) match {
            case None =>
            // TODO no duplicate runs in buffer
            case Some(targetStates) =>
              for (targetState <- targetStates)
                newBuffer += Run(targetState, run.obls :+ spawn(run.state, event, createdConjs))
          }
          //}
        }
        buffer = Some(newBuffer)
    }

    /*
     *T7: remove runs with violated obligations.
     */
    buffer = Some(buffer.get.filterNot(_.obls.exists(_.process(event, executedSubmons) == Bottom)))

    /*
     * T8: remove obligations that are met.
     */
    buffer = Some(buffer.get.map(run => Run(run.state, run.obls.filterNot(_.isEmpty))))

    /*
     * remove duplicate runs
     */
    buffer = Some(buffer.get.distinct)

    /*
     * T10: return if buffer has or hasn't some runs.
     */
    buffer.get.isEmpty
  }

  private def delta(q: State, event: Event): Option[Set[State]] = {
    if (q.interpret(struct, event, v)) {
      Some(q.targetStates)
    } else {
      None
    }
  }

  private def spawn(q: State, event: Event, createdForallConjs: mutable.HashMap[String, Conj]): Conj = {
    /*
     * Spawning conjunction: conjunction that contains universal-conjunctions and exists-disjunctions.
     */
    val bf = new Conj()

    q.formulae.foreach {
      case forall: Forall =>
        var conj = new Conj()
        // check if cached forall-conjunction exists
        createdForallConjs.get(forall.toString) match {
          case None =>
            // for all actions (p,d) of event
            for (action <- event if action._1 == forall.p.name) {
              // create new valuation
              var vNew: Valuation = v
              for ((variable, i) <- forall.p.args.zipWithIndex) {
                vNew += ((variable.name, action._2(i)))
              }
              // create sub-monitor
              conj.elems += new SAbasedMonitor(forall.phi, struct, vNew)
            }
            // store forall-conjunction in cache
            createdForallConjs.put(forall.toString, conj)
            // add forall-conjunction to spawning-conjunction
            bf.elems += conj
          case Some(forallConj) =>
            bf.elems += forallConj
        }
      // Exists-Disjunction
      case Not(psi) =>
        psi match {
          case forall: Forall =>
            val conj = new Conj()
            // check if cached forall-conjunction exists
            createdForallConjs.get(forall.toString) match {
              case None =>
                // for all actions (p,d) of event
                for (action <- event if action._1 == forall.p.name) {
                  // create new valuation
                  var vNew: Valuation = v
                  for ((variable, i) <- forall.p.args.zipWithIndex) {
                    vNew += ((variable.name, action._2(i)))
                  }
                  // create sub-monitor
                  conj.elems += new SAbasedMonitor(forall.phi, struct, vNew)
                }
                // store forall-conjunction in cache
                createdForallConjs.put(forall.toString, conj)
                // add negated forall-conjunction to spawning-conjunction
                bf.elems += new Neg(conj)
              case Some(forallConj) =>
                bf.elems += new Neg(forallConj)
            }
          case _ =>
        }
      case _ =>
    }
    bf
  }

  /*
   * log stuff
   */
  def monSize(bfHashSet: mutable.HashSet[BooleanFormula]) = buffer.getOrElse(mutable.Buffer()).toSeq.map(_.monSize(bfHashSet)).sum

  def numOfRuns = buffer.getOrElse(mutable.Buffer()).size

  def maxLengthOfRuns = buffer.getOrElse(mutable.Buffer()).toSeq.map(_.length).reduceOption(_ max _).getOrElse(0)

  def avgLengthOfRuns = buffer.getOrElse(mutable.Buffer()).toSeq.map(_.length).avg

  def maxOblLength = buffer.getOrElse(mutable.Buffer()).toSeq.map(_.obls.toSeq.map(_.length)).flatten.reduceOption(_ max _).getOrElse(0)

  def avgOblLength = buffer.getOrElse(mutable.Buffer()).toSeq.map(_.obls.toSeq.map(_.length)).flatten.avg

  def getMonitors = buffer.getOrElse(mutable.Buffer()).map(run => run.obls.map(obl => obl.getMonitors).flatten).toSet.flatten.asInstanceOf[Set[SAbasedMonitor]]

  override def hashCode = (this.toString + buffer.getOrElse(0).hashCode).hashCode
}

object SA {
  /*
    * global variable, pre-constructed SAs are loaded at runtime
    */
  var lookupTable: mutable.HashMap[Formula, Set[State]] = null

  def setLookupTable(policy: Formula) = {
    lookupTable = mutable.HashMap[Formula, Set[State]]()
    policy.getSubfsForSAcache.foreach { f => SA.lookupTable.put(f, new LBTParser(f).getStates)}
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
      lookupTable = Marshal.load[mutable.HashMap[Formula, Set[State]]](bytes)
    } catch {
      case e: FileNotFoundException => println(e.getMessage); sys.exit()
    }
  }

  def sizeLookupTable = lookupTable.values.toSeq.flatten.map(_.size).sum
}