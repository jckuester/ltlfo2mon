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
package ltlfo2mon.util.parsing

import ltlfo2mon.datatype.Types._
import ltlfo2mon.datatype._
import ltlfo2mon.monitor.unoptimised._
import ltlfo2mon.util.Printable._

import scala.collection.mutable
import scala.sys.process._
import scala.util.parsing.combinator._


class LBTParser(phi: Formula) extends JavaTokenParsers {
  private val statesMap: mutable.HashMap[String, State] = new mutable.HashMap[String, State]

  def getStates: Set[State] = {
    val lbtFormulaString = phi.toLBTstr(phi.encodedAtomsQ)
    val command = ("echo " + lbtFormulaString) #| "lbt" #| "lbt2dot"
    parse(command !!)

    /*
     * transition to state labeled
     * mark target states of state 0 as initial and delete initial state 0
     */
    statesMap.get("q0") match {
      case None => throw new Exception("LBTConverter.scala: State q0 does not exist.")
      case Some(q) => q.targetStates.foreach { state => state.isInitial = true}
    }
    statesMap.remove("q0")

    statesMap.values.toSet
  }

  /*
   * grammar: http://www.graphviz.org/doc/info/lang.html
   */
  def graph: Parser[Any] = "digraph" ~ ident ~ "{" ~ rep1(stmt) ~ "}"

  def stmt: Parser[Any] = (edge | state) ~ ";"

  def edge: Parser[Any] = id ~ "->" ~ id ~ label ^^ { case srcStateName ~ _ ~ targetStateName ~ formulae =>
    // if target state does not exist, create state
    statesMap.get("q" + targetStateName) match {
      case None => statesMap.put("q" + targetStateName, new State("q" + targetStateName, formulae, false))
      // add formulae from incoming transition to target state
      case Some(targetState) =>
        targetState.formulae ++= formulae
    }
    // if source state does not exist, create state
    statesMap.get("q" + srcStateName) match {
      case None =>
        val srcState = new State("q" + srcStateName, Set[Formula](), false)
        statesMap.put("q" + srcStateName, srcState)
        statesMap.get("q" + targetStateName) match {
          case None => throw new Exception("LBTConverter.scala: Target state 'q" + targetStateName + "' does not exist.")
          // add outgoing transition to state
          case Some(targetState) => srcState.targetStates += targetState
        }
      case Some(srcState) =>
        statesMap.get("q" + targetStateName) match {
          case None => throw new Exception("LBTConverter.scala: Target state 'q" + targetStateName + "' does not exist.")
          // add outgoing transition to state
          case Some(targetState) => srcState.targetStates += targetState
        }
    }
  }

  def state: Parser[Any] = id ~ "[" ~ """[a-z0-9=,\"\\]*""".r ~ "]"

  def label: Parser[Formulae] = "[label=\"" ~> gate <~ "\"]" ^^ { case gate => gate.literalsQ}

  // no disjunction: see http://www.tcs.hut.fi/Software/maria/tools/lbt/
  def gate: Parser[Formula] = and | negAtom | atom

  def and: Parser[Formula] = "&" ~> gate ~ gate ^^ { case gate1 ~ gate2 => And(gate1, gate2)}

  def negAtom: Parser[Formula] = "!" ~> atom ^^ { case gate => Not(gate)}

  def atom: Parser[Formula] = """t|(p[0-9]*)""".r ^^ { case atom if atom != "t" => phi.encodedAtomsQReverse.get(atom.drop(1).toInt).get
  case atom if atom == "t" => True
  }

  def id: Parser[String] = """[0-9]*""".r

  private def parse(dotString: String) = {
    parseAll(graph, dotString)
  }
}