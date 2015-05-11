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

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical._

class TraceParser(struct: Structure) extends StandardTokenParsers with PackratParsers {

  lexical.delimiters +=("(", ")", "{", "}", ",")

  def predParser: Parser[Elem] = elem("predicate", p => struct.uOps(p.chars))

  val action: Parser[Action] = predParser ~ "(" ~ rep1sep(domainValue, ",") <~ ")" ^^ { case name ~ _ ~ args => (name.chars, args.toVector)}
  val domainValue = numericLit | stringLit
  val event: Parser[Event] = "{" ~> repsep(action, ",") <~ "}" ^^ { case actions => actions.toSet}
  val trace: Parser[Trace] = rep1sep(event, ",") ^^ { case events => Trace(events)}

  def parse(input: String): Option[Trace] = {
    val tokens = new lexical.Scanner(input)
    phrase(trace)(tokens) match {
      case Success(result, _) => Some(result)
      case f: NoSuccess => println("Incorrect trace syntax: " + f.msg); None
    }
  }
}