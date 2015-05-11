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

import ltlfo2mon.Conf
import ltlfo2mon.datatype._

import scala.collection._
import scala.util.parsing.combinator.syntactical._

/*
 * Order of precedence: not = X = G = F > W > U > and > or > implies > iff > forall = exists,
 *                      bind from right to left, e.g., p U (q U r)  
 */
class FormulaParser(struct: Structure) extends StandardTokenParsers with OperatorPrecedenceParsers {
  var boundVariables: mutable.Set[String] = null

  lexical.delimiters += ("!", "?", "~", "[]", "<>", """/\""", "&", """\/""", "|", "->", "<->", ":", ".", "(", ")", ",")
  lexical.reserved += ("U", "X", "A", "E", "G", "F", "W", "true", "false")
      
  lazy val variable = varNameParser ^^ { case v => Var(v) }
  lazy val const = constNameParser ^^ { case c => Const(c) }
  lazy val funct = functNameParser ~ "(" ~ terms <~ ")" ^^ { case f ~ _ ~ vector => Funct(f, vector) }
  lazy val terms: Parser[Vector[Term]] = rep1sep(variable | const | funct, ",") ^^ { case list => list.toVector }
  val args = ("(" ~> terms <~ ")").? ^^ { terms => terms }

  lazy val quantifierVars = "(" ~> rep1sep(newVarNameParser, ",") <~ ")" ^^
    { case list => list.map(x => Var(x)).toVector } | newVarNameParser ^^ { case x => Vector(Var(x)) }

  lazy val uOp = uOpNameParser ~ args ^^ { case name ~ attrs => attrs match {
    // syntactic sugar: propositions (predicates without args)
    case None => Uop(name, Vector())
    case Some(vector) => Uop(name, vector)
  } }
  lazy val iOp = iOpNameParser ~ args ^^ { case name ~ attrs => attrs match {
    // syntactic sugar: propositions (predicates without args)
    case None => Iop(name, Vector(), struct.iOps(name)._2)
    case Some(vector) => Iop(name, vector, struct.iOps(name)._2)
  } }

  val top = "true" ^^ { case _ => True }
  val bottom = "false" ^^ { case _ => False } // Not(True())
  val forall = ("A" | "!") ~! quantifierVars ~ ":" ~ uOpNameParser <~ "." ^^
      { case _ ~ vector ~ _ ~ uOpName => Uop(uOpName, vector) }
  val exists = ("E" | "?") ~> quantifierVars ~ ":" ~ uOpNameParser <~ "." ^^
    { case vector ~ _ ~ uOpName => Uop(uOpName, vector) }

  def formula: Parser[Formula] = operators[Any, Formula](
    Prefix(100)("~") { (_, phi) => Not(phi) },
    Prefix(100)("X") { (_, phi) => Next(phi) },
    Prefix(100)("G" | "[]") { (_, phi) => Globally(phi) }, // Not(Until(True(), Not(phi)), true)
    Prefix(100)("F" | "<>") { (_, phi) => Eventually(phi) }, // Until(True(), phi, true)
    Infix(200, 200-1)("W") { (_, phi, psi) => Or(Until(phi, psi), Globally(phi)) }, // WeakUntil(phi, psi)
    Infix(300, 300-1)("U") { (_, phi, psi) => Until(phi, psi) },
    Infix(400-1, 400)("&" | """/\""") { (_, phi, psi) => And(phi, psi) },
    Infix(500-1, 500)("|" | """\/""") { (_, phi, psi) => Or(phi, psi) },
    Infix(600-1, 600)("->") { (_, phi, psi) => Or(Not(phi), psi) }, // Implies(phi, psi)
    Infix(700-1, 700)("<->") { (_, phi, psi) => Or(And(phi, psi), And(Not(phi), Not(psi))) },
    Prefix(800)(forall) { (uOpX, phi) => Forall(uOpX, phi) },
    Prefix(800)(exists) { (uOpX, phi) => Exists(uOpX, phi) } // Not(Forall(uOpX, phi))
  )("(" ~> formula <~ ")" | top | bottom | iOp | uOp)

  def constNameParser = elem("", c => {
    if(struct.consts.keySet(c.toString)) {
      true
    } else if (c.toString.matches("\".*\"")) {
      // add strings (i.e., characters surrounded by ', or ") automatically as constants
      // note: .chars removes quotes (' and "), but we want to keep them, thus we use .toString for constant identifier
      Conf.struct.addConst(c.toString, c.chars, false)
    } else if (c.toString.matches("""\d*""")) {
      // add integers automatically as constants
      Conf.struct.addConst(c.toString, c.toString.toInt, false)
    } else false }) ^^ { case c => c.toString }

  def varNameParser = elem("", v => boundVariables(v.chars)) ^^ { case v => v.chars }
  def functNameParser = elem("function or constant", f => struct.functs.keySet(f.chars)) ^^ { case f => f.chars }
  // allow arbitrary U-Op names (if disjoint with other names used in struct)
  def uOpNameParser = elem("uninterpreted predicate (U-Op)", p => { if(struct.uOps(p.chars)) true else {
    struct.addUoperator(p.chars, false) }}) ^^ { case p => p.chars }
  def iOpNameParser = elem("predicate (interpreted (I-Op) or uninterpreted (U-Op))",
    r => struct.iOps.keySet(r.chars)) ^^ { case r => r.chars }
  // allow reuse of '_' as placeholder for variable names
  lazy val newVarNameParser =  elem("", v => {
    if (boundVariables(v.chars)) false
    else if(v.chars == "_") true
    else if(struct.vars(v.chars) || struct.addVar(v.chars, false)) { boundVariables += v.chars; true }
    else false }) ^^ { case v => v.chars }


  def parse(input: String): Option[Formula] = {
    boundVariables = mutable.Set[String]()

    val tokens = new lexical.Scanner(input)
    phrase(formula)(tokens) match {
      case Success(result, _) => Some(result)
      // TODO improve parser error msg
      //case f: NoSuccess => println("Incorrect formula syntax: " + f.msg); None
      case f: NoSuccess => println("Incorrect formula."); None
    }
  }
}