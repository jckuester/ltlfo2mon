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
import ltlfo2mon.monitor.optimised.{States, _}
import ltlfo2mon.util.Printable._

import scala.collection.mutable
import scala.sys.process._
import scala.util.parsing.combinator._

/**
 * Takes a formula as input,
 * applies the ltl3-tool to it and outputs an automaton (datatype.automaton)
 *
 * @param phi: formula
 *
 */
class LTL3ToolsParser(phi: Formula) extends JavaTokenParsers {
  private val statesByName: mutable.HashMap[String, State] = new mutable.HashMap[String, State]

  /*
   *  adds state to "statesByName" if it not exists
   */
  private def getState(stateName: String): State = {
    statesByName.get(stateName) match {
      case None =>
        val state = State("q" + statesByName.size)
        if (stateName.equals("00")) {
          state.isInitial = true
        }
        statesByName.put(stateName, state)
        state
      case Some(state) => state
    }
  }

  /*
   * output of ltl3tool is in dot format
   * grammar: http://www.graphviz.org/doc/info/lang.html
   */
  private def graph: Parser[Any] = "digraph G {" ~ rep1(stmt) ~ "}"

  private def stmt: Parser[Any] = transition | state

  private def transition: Parser[Any] = stateName ~ "->" ~ stateName ~ transitionLabel ^^ {
    case srcStateName ~ _ ~ targetStateName ~ atoms =>
      // add negated atoms explicitly to guards
      val atomsExplicit = atoms ++ (phi.encodedAtomsQ.keySet -- atoms).map(f => Not(f))
      getState(srcStateName).transitions += new Transition(getState(targetStateName), atomsExplicit)
  }

  private def stateName: Parser[String] = "\"(" ~> id ~ "," ~ id <~ ")\"" ^^ {
    case id1 ~ _ ~ id2 => id1 + id2
  }

  private def id: Parser[String] = """[-0-9]*""".r

  private def transitionLabel: Parser[Formulae] = "[label = \"(" ~> gate <~ ")\"];"

  private def gate: Parser[Formulae] = and | atom | empty

  private def and: Parser[Formulae] = atom ~ "&&" ~ gate ^^ { case atom ~ _ ~ gate => gate ++ atom}

  private def atom: Parser[Formulae] = """true|false|(p[0-9]*)""".r ^^ {
    case atom if atom == "true" => Set[Formula](True)
    case atom if atom == "false" => Set[Formula](False)
    case atom => Set[Formula](phi.encodedAtomsQReverse.get(atom.drop(1).toInt).get)
  }

  private def empty: Parser[Formulae] = "<empty>".r ^^ { case _ => Set[Formula]()}

  private def state: Parser[Any] = stateName ~ stateLabelColor ^^ {
    case stateLTL3Name ~ stateLabel =>
      getState(stateLTL3Name).ltl3type = {
        if (stateLabel.equals("green")) Top
        else if (stateLabel.equals("red")) Bottom
        else Unknown
      }
  }

  private def stateLabelColor: Parser[String] = "[" ~> """[-a-z0-9=,\"\(\)\s]*color=""".r ~ ident <~ "]" ^^ { case _ ~ color => color}

  def parse(): States = {
    val ltl3toolFormulaString = phi.toLTL3ToolsStr(phi.encodedAtomsQ)
    val command = Seq("ltl3tools.sh", ltl3toolFormulaString)
    parseAll(graph, command !!)

    new States(statesByName.values.toSet).optimise
  }
}
