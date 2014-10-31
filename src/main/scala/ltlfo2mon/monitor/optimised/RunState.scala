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

import ltlfo2mon.datatype.Types._
import ltlfo2mon.datatype._
import scala.collection._
import scala.collection.mutable.ListBuffer

case class RunState(state: State, var runTransitions: mutable.Set[RunTransition] = mutable.Set()) {

  def toString(indent: Int) = state.name + "(" + state.ltl3type + ")\n" + {
    "\t" * (indent + 2)
  } + runTransitions.map(t => t.toString(indent + 2)).mkString("\n" + "\t" * (indent + 2))

  /**
   * 1) processes all obls in run tree
   * 2) prunes runs that have violated obls
   * 3) removes obls that are met from runs
   *
   * returns leaves of subtree (if this $runstate is root then all leaves)
   */
  def process(event: Event, cachedBFormulae: mutable.HashSet[BooleanFormula]): ListBuffer[RunState] = {
    var leaves: ListBuffer[RunState] = ListBuffer()
    var newRunTransitions: mutable.Set[RunTransition] = mutable.Set()

    if (runTransitions.isEmpty) {
      leaves += this // this obligation is a leaf
    } else {
      do {
        val tmpRunTransitions: mutable.Set[RunTransition] = mutable.Set()

        runTransitions.foreach {
          runTransition =>
            val (obl, result) = BooleanFormula.process(runTransition.obl, event, cachedBFormulae)
            runTransition.obl = obl.asInstanceOf[Conj]
            result match {
              case Bottom => // prune sub run tree
              // target state of run transition is NOT leaf
              case Top if runTransition.targetRunState.runTransitions.nonEmpty =>
                // remove intermediate existingRunTransition
                tmpRunTransitions ++= runTransition.targetRunState.runTransitions
              case _ => newRunTransitions += runTransition
            }
        }
        runTransitions.clear()
        runTransitions ++= tmpRunTransitions
      } while (runTransitions.nonEmpty)
    }

    // recursively process obls in child nodes
    newRunTransitions.foreach {
      newRunTransition => leaves ++= newRunTransition.targetRunState.process(event, cachedBFormulae)
    }
    runTransitions = newRunTransitions

    leaves
  }

  /**
   * count '1' per transition
   */
  def lengthOfRuns: Seq[Int] = {
    if (runTransitions.isEmpty) {
      // leaf state
      Seq(0)
    } else {
      runTransitions.toSeq.map(_.targetRunState.lengthOfRuns.map(_ + 1)).flatten
    }
  }

  def monSize(bfHashSet: mutable.HashSet[BooleanFormula]): Int = {
    if (runTransitions.isEmpty) {
      // leaf state (stores ltl3type or if Unknown() the state name)
      1
    } else {
      runTransitions.toSeq.map(t => t.obl.monSize(bfHashSet) + t.targetRunState.monSize(bfHashSet)).sum
    }
  }

  override def hashCode = 41 * state.name.hashCode() + runTransitions.toSeq.map(_.hashCode()).sum
}