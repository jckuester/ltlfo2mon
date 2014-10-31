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
package ltlfo2mon.util.parsing

import ltlfo2mon.datatype._

import scala.collection._
import scala.util.parsing.combinator.syntactical._
/*
 * Order of precedence: not = X = G = F > W > U > and > or > implies > iff > forall = exists,
 *                      bind from right to left, e.g., p U (q U r)  
 */

// TODO check for no free variables
class FormulaParser(struct: Structure) extends StandardTokenParsers with OperatorPrecedenceParsers {
  var variables: mutable.Set[String] = null
  
  lexical.delimiters += ("!", "[]", "<>", "&&", "&", "||", "|", "->", "<->", ":", ".", "(", ")", "/", "*", ",")
  lexical.reserved += ( "U", "X", "A", "E", "G", "F", "W", "true", "false")
      
  lazy val variable = varNameParser ^^ { case v => Var(v.chars)}
  lazy val const = constNameParser ^^ { case c => Const(c.chars)} 
  lazy val funct = functNameParser ~ "(" ~ terms <~ ")" ^^ { case f ~ _ ~ vector => Funct(f.chars, vector)}
  lazy val terms: Parser[Vector[Term]] = rep1sep(variable | const | funct, ",") ^^ {case list => list.toVector }
  val args = ("(" ~> terms <~ ")").? ^^ { terms => terms }
  lazy val vars = newVarParser ^^ { case x => variables += x.chars; Vector(Var(x.chars)) } | "(" ~> rep1sep(newVarParser, ",") <~ ")" ^^ { case list => variables ++= list.map(_.chars).toSet; list.map(x => Var(x.chars)).toVector
                                                                                    }
  lazy val newVarParser: Parser[Elem] = elem("forallVar", v => struct.isFreeName(v.chars))
  
  lazy val uOp = uOpNameParser ~ args ^^ { case name ~ attrs => Uop(name.chars, attrs.get) }
  lazy val iOp = iOpNameParser ~ args ^^ { case name ~ attrs => Iop(name.chars, attrs.get, struct.iOps(name.chars)._2) } 
  val top = "true" ^^ {case _ => True}
  val bottom = "false" ^^ {case _ => False} // Not(True())
  val forall = "A" ~> vars ~ ":" ~ uOpNameParser <~"." ^^ { case vector ~ _ ~ uOpName =>  Uop(uOpName.chars, vector)}
  val exists = "E" ~> vars ~ ":" ~ uOpNameParser <~"." ^^ { case vector ~ _ ~ uOpName =>  Uop(uOpName.chars, vector)}
  
  def formula:Parser[Formula] = operators[Any,Formula](
      Prefix(100)("!"|"not") { (_, phi) => Not(phi) },
      Prefix(100)("X") { (_, phi) => Next(phi) },
      Prefix(100)("G"|"[]") { (_, phi) => Globally(phi) }, // Not(Until(True(), Not(phi)), true)
      Prefix(100)("F"|"<>") { (_, phi) => Eventually(phi) },  // Until(True(), phi, true)
      Infix(200, 200-1)("W") { (_, phi, psi) => Or(Until(phi, psi),Globally(phi)) },  // WeakUntil(phi,psi)
      Infix(300, 300-1)("U") { (_, phi, psi) => Until(phi, psi) },
      Infix(400-1, 400)("&"|"&&") { (_, phi, psi) => And(phi, psi) },
      Infix(500-1, 500)("|"|"||") { (_, phi, psi) => Or(phi, psi) },
      Infix(600-1, 600)("->") { (_, phi, psi) => Or(Not(phi), psi) }, // Implies(phi, psi)
      //Infix(700-1, 700)("<->") { (_, phi, psi) => And(Implies(phi, psi), Implies(psi, phi)) },
      Prefix(800)(forall) { (uOpX,phi) => Forall(uOpX, phi) },
      Prefix(800)(exists) { (uOpX,phi) => Exists(uOpX, phi) } // Not(Forall(uOpX, phi))      
      ) ( "(" ~> formula <~ ")" | top | bottom | uOp | iOp )
     
  def varNameParser: Parser[Elem] = elem("variable", v => variables(v.chars))
  def constNameParser: Parser[Elem] = elem("constant", c => struct.consts.keySet(c.chars))
  def functNameParser: Parser[Elem] = elem("function", f => struct.functs.keySet(f.chars))
  def uOpNameParser: Parser[Elem] = elem("uOp", p => struct.uOps(p.chars))
  def iOpNameParser: Parser[Elem] = elem("iOp", r => struct.iOps.keySet(r.chars))
   
  /**
   * returns a formula with no double negation (..!(!...)..)
   */
  def parse(input: String): Option[Formula] = {
    variables = mutable.Set[String]()
    val tokens = new lexical.Scanner(input)
    phrase(formula)(tokens) match {
      case Success(result, _) => Some(result)     
      case f: NoSuccess => println("Incorrect formula syntax: " + f.msg); None     
    }
  }
}
