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

package ltlfo2mon.datatype

import Types._
import scala.collection._

sealed abstract class Formula {
  val length: Int
  val level: Int

  override def toString = toString(asTex = false)

  def toTex = toString(asTex = true)

  def toString(asTex: Boolean): String

  /*
   * quantified sub-formulae are considered as atoms
   */
  val isAtomQ: Boolean
  val isLiteralQ: Boolean
  val subFormulae: Formulae
  lazy val closure: Formulae = subFormulae union subFormulae.map { f => Not(f)}
  val subFormulaeQ: Formulae // subformulae with quantified formulae treated as atoms
  lazy val closureQ: Formulae = subFormulaeQ union subFormulaeQ.map { f => Not(f)}
  val numOfPreds: Int
  lazy val encodedAtomsQ: mutable.HashMap[Formula, Int] = mutable.HashMap[Formula, Int]() ++ closureQ.filter(_.isAtomQ).zipWithIndex
  lazy val encodedAtomsQReverse: mutable.HashMap[Int, Formula] = encodedAtomsQ.map(_.swap)

  /*
   * get quantified sub-formulae without quantifier 
   */
  def getPhiOfQuantifiedSFs: Formulae = closure.filter(_.isInstanceOf[Forall]).asInstanceOf[Set[Forall]].map(forall => forall.phi) union Set(this)

  def getSubfsForSAcache: Formulae = getPhiOfQuantifiedSFs union getPhiOfQuantifiedSFs.map(f => Not(f))

  def literalsQ: Formulae

  lazy val atomsQ: Formulae = literalsQ.filter(_.isAtomQ)
  lazy val showParentheses = !(this.isInstanceOf[UnOp] || this.isInstanceOf[Pred] || this == True || this == False)
}

/*
 * ####
 * Predicate. Two types of predicates: U- and I-operators.
 * ####
 */
sealed abstract class Pred(name: String, args: Vector[Term]) extends Formula {
  override def toString(asTex: Boolean) = name + "(" + args.mkString(", ") + ")"

  val length = 1
  val level = 0
  val isAtomQ = true
  val isLiteralQ = true
  val subFormulae = Set[Formula](this)
  val subFormulaeQ = Set[Formula](this)
  val numOfPreds = 1
  val numOfQuantified = 0

  def literalsQ = Set[Formula](this)
}

/*
 * U-operator: uninterpreted predicate.
 */
case class Uop(name: String, args: Vector[Term]) extends Pred(name, args) {
  def interpret(struct: Structure, event: Event, v: Valuation) = if (event((this.name, args.map(_.interpret(struct, v).get.toString)))) true else false
}

/*
 * I-operator: interpreted predicate.
 */
case class Iop(name: String, args: Vector[Term], isRigid: Boolean) extends Pred(name, args) {
  def interpret(struct: Structure, v: Valuation) = if (struct.iOps(name)._1(args.map(_.interpret(struct, v).get))) true else false
}

/*
 * ##############
 * Unary operator
 * ##############
 */
sealed abstract class UnOp(phi: Formula) extends Formula {
  val operatorSymbol: String
  val texSymbol: String

  def toString(asTex: Boolean) = {
    if (asTex) texSymbol else operatorSymbol
  } + {
    if (phi.showParentheses) "(" + phi.toString(asTex) + ")" else phi.toString(asTex)
  }

  val length = phi.length + 1
  val isAtomQ = if (this.isInstanceOf[Forall]) true else false
  val isLiteralQ = if ((this.isInstanceOf[Not] && phi.isAtomQ) || this.isAtomQ) true else false
  val numOfPreds = phi.numOfPreds
  val numOfQuantified = if (this.isInstanceOf[Forall]) 1 else 0

  def literalsQ = if (this.isLiteralQ) Set[Formula](this) else Set[Formula]()
}

class Not(val phi: Formula) extends UnOp(phi) {
  val operatorSymbol = "\u00AC"
  val texSymbol = """\lnot """
  val level = phi.level
  lazy val subFormulae = phi.subFormulae + this
  lazy val subFormulaeQ = phi.subFormulaeQ + this

  override def equals(that: Any): Boolean = {
    that.isInstanceOf[Not] && (this.hashCode() == that.asInstanceOf[Not].hashCode())
  }

  override def hashCode = 41 + phi.hashCode()
}

object Not {
  def apply(phi: Formula) = phi match {
    case True => False
    case False => True
    // eliminate nested negations
    case not: Not => not.phi
    case _ => new Not(phi)
  }

  def unapply(not: Not): Option[Formula] = Some(not.phi)
}

case class Next(phi: Formula) extends UnOp(phi) {
  val operatorSymbol = "X"
  val texSymbol = """\ltlX """
  val level = phi.level
  lazy val subFormulae = phi.subFormulae + this
  lazy val subFormulaeQ = phi.subFormulaeQ + this
}

case class Forall(p: Uop, phi: Formula) extends UnOp(phi) {
  require(p.args.forall(_.isInstanceOf[Var]))
  val operatorSymbol = "\u2200" + p.args.mkString(",") + ":" + p.name + "."
  val texSymbol = """\forall """ + p.args.mkString(",") + ":" + p.name + """.\ """
  val level = phi.level + 1
  lazy val subFormulae = phi.subFormulae + this
  lazy val subFormulaeQ = Set[Formula](this)
}

/*
 * ###############
 * Binary operator
 * ###############
 */
sealed abstract class BiOp(phi: Formula, psi: Formula) extends Formula {
  val operatorSymbol: String
  val texSymbol: String

  def toString(asTex: Boolean) = {
    {
      if (phi.showParentheses) "(" + phi.toString(asTex) + ") " else phi.toString(asTex) + " "
    } + {
      if (asTex) texSymbol else operatorSymbol
    } + {
      if (psi.showParentheses) " (" + psi.toString(asTex) + ")" else " " + psi.toString(asTex)
    }
  }

  val length = phi.length + psi.length + 1
  val level = math.max(phi.level, psi.level)
  val isAtomQ = false
  val isLiteralQ = false
  lazy val subFormulae = phi.subFormulae ++ psi.subFormulae + this
  lazy val subFormulaeQ = phi.subFormulaeQ ++ psi.subFormulaeQ + this
  val numOfPreds = phi.numOfPreds + psi.numOfPreds

  def literalsQ: Formulae = phi.literalsQ ++ psi.literalsQ
}

case class And(phi: Formula, psi: Formula) extends BiOp(phi, psi) {
  val operatorSymbol = "\u2227"
  val texSymbol = """\land"""

  def eval = {
    phi match {
      case True => psi match {
        case True => True
        case False => False
        case _ => psi
      }
      case False => False
      case _ => psi match {
        case True => phi
        case False => False
        case _ => this
      }
    }
  }
}

case class Or(val phi: Formula, val psi: Formula) extends BiOp(phi, psi) {
  val operatorSymbol = "\u2228"
  val texSymbol = """\lor"""

  def eval = {
    phi match {
      case True => True
      case False => psi match {
        case True => True
        case False => False
        case _ => psi
      }
      case _ => psi match {
        case True => True
        case False => phi
        case _ => this
      }
    }
  }

  /*
  override def equals(that: Any): Boolean = {
    that.isInstanceOf[Or] && (this.hashCode() == that.asInstanceOf[Or].hashCode())
  }

  override def hashCode = (2 * 41 + phi.hashCode()) * 41 + psi.hashCode()
  */
}

/*
object Or {
  def apply(phi: Formula, psi: Formula) = phi match {
    case not: Not => new Implies(not.phi, psi)
    case _ => new Or(phi, psi)
  }

  def unapply(or: Or): Option[(Formula, Formula)] = Some(or.phi, or.psi)
}
*/

case class Until(phi: Formula, psi: Formula) extends BiOp(phi, psi) {
  val operatorSymbol = "U"
  val texSymbol = """\ltlU"""
}

/*
object Until {
  def apply(phi: Formula, psi: Formula) = new Until(phi, psi)

  def unapply(until: Until): Option[(Formula, Formula)] = Some(until.phi, until.psi)
}
*/

case object True extends Formula {
  def toString(asTex: Boolean) = if (asTex) """\top""" else "\u22A4"

  val length = 1
  val level = 0
  val isAtomQ = false
  val isLiteralQ = false
  val subFormulae = Set[Formula](this)
  val subFormulaeQ = Set[Formula](this)
  val numOfPreds = 0
  val numOfQuantified = 0

  def literalsQ = Set[Formula]()
}

/*
 * ###############
 * Syntactic sugar
 * ###############
 */

case class Eventually(phi: Formula) extends UnOp(phi) {
  // extends Until(True(), chi)
  val operatorSymbol = "F"
  val texSymbol = """\ltlF """
  val level = phi.level
  lazy val subFormulae = phi.subFormulae + this
  lazy val subFormulaeQ = phi.subFormulaeQ + this
}

case class Globally(phi: Formula) extends UnOp(phi) {
  // extends Not(Until(True(), Not(chi)))
  val operatorSymbol = "G"
  val texSymbol = """\ltlG """
  val level = phi.level
  lazy val subFormulae = phi.subFormulae + this
  lazy val subFormulaeQ = phi.subFormulaeQ + this
}

/*
case class Implies(chi: Formula, xi: Formula) extends Or(Not(chi), xi) {
  override val operatorSymbol = "\u21d2"
  override val texSymbol = """\Rightarrow"""

  override def toString(asTex: Boolean) = {
    if (chi.showParentheses) "(" + chi.toString(asTex) + ") " else chi.toString(asTex) + " "
  } + {
    if (asTex) texSymbol else operatorSymbol
  } + {
    if (xi.showParentheses) " (" + xi.toString(asTex) + ")" else " " + xi.toString(asTex)
  }

  override val length = chi.length + xi.length + 1
  override lazy val subFormulae = chi.subFormulae ++ xi.subFormulae + this
  override lazy val subFormulaeQ = chi.subFormulaeQ ++ xi.subFormulaeQ + this
}
*/

/*
case class WeakUntil(chi: Formula, xi: Formula) extends Or(Until(chi, xi), Globally(chi)) {
  override val operatorSymbol = "W"
  override val texSymbol = """\ltlW"""

  override def toString(asTex: Boolean) = {
    if (chi.showParentheses) "(" + chi.toString(asTex) + ") " else chi.toString(asTex) + " "
  } + {
    if (asTex) texSymbol else operatorSymbol
  } + {
    if (xi.showParentheses) " (" + xi.toString(asTex) + ")" else " " + xi.toString(asTex)
  }

  override val length = chi.length + xi.length + 1
  override lazy val subFormulae = chi.subFormulae ++ xi.subFormulae + this
  override lazy val subFormulaeQ = chi.subFormulaeQ ++ xi.subFormulaeQ + this
}
*/

/*
case class Release(phi: Formula, psi: Formula) extends Formula  {
  override def toString = phi + " R " + psi    
  def length = phi.length + psi.length + 1
  val isAtom = false
  val isLiteral = false
  val level = math.max(phi.level, psi.level)
  def subst(term: Any, variable: Var) = { Release(phi.subst(variable, term), psi.subst(variable, term)) }
  def subFormulae =  phi.subFormulae ++ psi.subFormulae + this
  def subformulaeQ = phi.subformulaeQ ++ psi.subformulaeQ + this
}
*/

case class Exists(p: Uop, chi: Formula) extends Not(Forall(p, Not(chi))) {
  require(p.args.forall(_.isInstanceOf[Var]))
  override val operatorSymbol = "\u2203" + p.args.mkString(",") + ":" + p.name + "."
  override val texSymbol = """\exists """ + p.args.mkString(",") + ":" + p.name + """.\ """

  override def toString(asTex: Boolean) = {
    if (asTex) texSymbol else operatorSymbol
  } + {
    if (chi.showParentheses) "(" + chi.toString(asTex) + ")" else chi.toString(asTex)
  }

  override val length = chi.length + 1
  override lazy val subFormulae = chi.subFormulae + this
  lazy val subformulaeQ = Set[Formula](this)
}

case object False extends Formula {
  override def toString(asTex: Boolean) = if (asTex) """\bot""" else "\u22A5"

  override val length = 1
  override val level = 0
  override lazy val subFormulae = Set[Formula](this)
  override lazy val subFormulaeQ = Set[Formula](this)
  override val numOfPreds = 0
  override val isAtomQ = false
  override val isLiteralQ = false

  override def literalsQ = Set[Formula]()
}

// used only by progression
case class ForallConj(elems: Set[(Formula, Valuation)] = Set[(Formula, Valuation)]()) extends Formula {
  def toString(asTex: Boolean) = if (elems.isEmpty) ""
  else elems.toSeq.map { case (f, v) => f + {
    if (v.isEmpty) "" else v.mkString("{", ", ", "}")
  }
  }.mkString(" \u2227 ")

  val length = elems.toSeq.map {
    _._1.length
  }.sum + {
    if (elems.size > 0) elems.size - 1 else 0
  }
  val level = elems.toSeq.map(_._1.level).reduceOption(_ max _).getOrElse(0)
  val isAtomQ = false
  val isLiteralQ = false
  val subFormulae = elems.map(_._1.subFormulae).fold(Set())(_ union _)
  val subFormulaeQ = elems.map(_._1.subFormulaeQ).fold(Set())(_ union _)
  val numOfPreds = elems.map(_._1.numOfPreds).sum

  def literalsQ = elems.map(_._1.literalsQ).fold(Set())(_ union _)

  def eval = {
    val newElems = elems.filterNot(_._1 == True)
    if (newElems.exists(_._1 == False)) False
    else if (newElems.isEmpty) True
    else ForallConj(newElems)
  }
}
