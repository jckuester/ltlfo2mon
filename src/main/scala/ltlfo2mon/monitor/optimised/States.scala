/*******************************************************************************
 * This is part of ltlfo2mon (https://github.com/jckuester/ltlfo2mon).
 *  
 *  Copyright (c) 2013 by Jan-Christoph Kuester <kuester@sdf.org>
 *   
 *  Ltlfo2mon is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *   
 *  Ltlfo2mon is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *   
 *  You should have received a copy of the GNU General Public License
 *  along with ltlfo2mon.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package ltlfo2mon.monitor.optimised

import java.util

import sys.process._
import ltlfo2mon.datatype._

class States(val states: Set[State]) extends util.HashSet[State] {

  /*
   * optimise states of LTL3tools output
   */
  def optimise: States = {
    // remove all transitions from "red" and "green" states
    states.filter(s => s.ltl3type match {
      case Top => true
      case Bottom => true
      case Unknown => false
    }).foreach(s => s.transitions = Set[Transition]())

    new States(states)
  }

  lazy val initialState = states.find(_.isInitial).get

  def toDot: String = {
    val statesToIndex = states.zipWithIndex.toMap

    def getIndexOfState(state: State) = statesToIndex.get(state) match {
      case None => throw new Exception("state " + state.hashCode + " not found in statesToIndex: " + statesToIndex)
      case Some(stateIndex) => stateIndex
    }

    println("### toDot(): ### ")
    states.foreach { s => s.transitions.foreach { t => println("\t" + s + t) } }
    println()

    "digraph graphname {\n" +
      "\"\" [shape=none]\n" +
      // draw states
      states.map { s =>
        getIndexOfState(s) +
          " [label=\"" + s + "\"" + {
            s.ltl3type match {
              case Top => ",style=filled,color=green"
              case Bottom => ",style=filled,color=red"
              case Unknown => ",style=filled,color=yellow"
            }
          } + "];"
      }.mkString("\n") + "\n\n" +
      // draw initial states
      "\"\" -> " + getIndexOfState(initialState) + ";\n" +
      // draw transitions
      states.map { s =>
        s.transitions.map { t =>
          getIndexOfState(s) +
            " -> " + getIndexOfState(t.targetState) + " [label=\"" +
            t.guard.map { f => f.toTex }.mkString(", ") +
            "\"];"
        }.mkString("\n")
      }.mkString("\n") +
      "\n}"
  }

  def showDot = {
    (Seq("echo", this.toDot) #| "dotty -").run()
  }
  
  override def size = states.toSeq.map(_.size).sum
}