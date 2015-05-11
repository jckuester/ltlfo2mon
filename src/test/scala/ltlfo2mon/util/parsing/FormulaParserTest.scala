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

import ltlfo2mon.Conf
import ltlfo2mon.datatype._
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test

class FormulaParserTest extends AssertionsForJUnit {

  @Test
  def testParseGloballyForall() {
    val formula = "G A x:w. even(x)"

    new FormulaParser(Conf.struct).parse(formula) match {
      case None => assert(false)
      case Some(formula) => assertEquals(Globally(Forall(Uop("w", Vector(Var("x"))),
        Iop("even", Vector(Var("x")), false))), formula)
    }
  }

  @Test
  def testParseAndOrPrecedence() {
    val formula = """even('5') \/ eq('5', '4') /\ leq('3', '4')"""

    new FormulaParser(Conf.struct).parse(formula) match {
      case None => assert(false)
      case Some(formula) => {
        assertEquals(Or(Iop("even", Vector(Const("\"5\"")), false),
          And(Iop("eq", Vector(Const("\"5\""), Const("\"4\"")), false),
            Iop("leq", Vector(Const("\"3\""), Const("\"4\"")), false))), formula)

        assertEquals(3, Conf.struct.consts("\"3\""))
        assertEquals(4, Conf.struct.consts("\"4\""))
        assertEquals(5, Conf.struct.consts("\"5\""))
      }
    }
  }

  @Test
  def testParseWrongUop() {
    val formula = "G A x:even. blub(x)"

    new FormulaParser(Conf.struct).parse(formula) match {
      case None => assert(true); println()
      case Some(formula) => assert(false)
    }
  }

  @Test
  def testParseFreeVar() {
    val formula = "blub(x)"

    new FormulaParser(Conf.struct).parse(formula) match {
      case None => assert(true); println()
      case Some(formula) => assert(false)
    }
  }

  @Test
  def testParseStringConstant() {
    val formula = "blub(\'x\')"

    new FormulaParser(Conf.struct).parse(formula) match {
      case None => assert(false)
      case Some(formula) => {
        assertEquals(Uop("blub", Vector(Const("\"x\""))), formula)
        assertEquals("x", Conf.struct.consts("\"x\""))
      }
    }
  }

  @Test
  def testParseIntegerConstant() {
    val formula = "blub(19)"

    new FormulaParser(Conf.struct).parse(formula) match {
      case None => assert(false)
      case Some(formula) => {
        assertEquals(Uop("blub", Vector(Const("19"))), formula)
        assertEquals(19, Conf.struct.consts("19"))
      }
    }
  }

  @Test
  def testParseNestedForallVariableNotDisjoint() {
    val formula = "G A x:w. A x :w. even(x)"

    new FormulaParser(Conf.struct).parse(formula) match {
      case None => assert(true); println()
      case Some(formula) => assert(false)
    }
  }

  @Test
  def testParseForallStringConstant() {
    val formula = """G A z:ww. even("y")"""

    new FormulaParser(Conf.struct).parse(formula) match {
      case None => assert(false)
      case Some(formula) =>  assertEquals(Globally(Forall(Uop("ww", Vector(Var("z"))),
        Iop("even", Vector(Const("\"y\"")), false))), formula)
    }
  }

  @Test
  def testParseNestedForallVariables() {
    val formula = "G A x:w. A y :w. even(x)"

    new FormulaParser(Conf.struct).parse(formula) match {
      case None => assert(false)
      case Some(formula) => assertEquals(Globally(Forall(Uop("w", Vector(Var("x"))), Forall(Uop("w", Vector(Var("y"))),
        Iop("even", Vector(Var("x")), false)))), formula)
    }
  }

  @Test
  def testParseKeywordVariable() {
    val formula = "G A true:w. A y :w. even(y)"

    new FormulaParser(Conf.struct).parse(formula) match {
      case None => assert(true)
      case Some(formula) => assert(false)
    }
  }

  @Test
  def testParseKeywordUopA() {
    val formula = "A y :w. A(y)"

    new FormulaParser(Conf.struct).parse(formula) match {
      case None => assert(true)
      case Some(formula) => assert(false)
    }
  }

  @Test
  def testParseFunction() {
    val formula = "G A x :v. even(add(x, x))"

    new FormulaParser(Conf.struct).parse(formula) match {
      case None => assert(false)
      case Some(formula) => assertEquals(Globally(Forall(Uop("v", Vector(Var("x"))),
        Iop("even", Vector(Funct("add", Vector(Var("x"), Var("x")))), false))), formula)
    }
  }

  @Test
  def testParseVarBoundTwice() {
    val formula = "A (y, y):w. even(y)"

    new FormulaParser(Conf.struct).parse(formula) match {
      case None => assert(true)
      case Some(formula) => assert(false)
    }
  }

  @Test
  def testParseVarPlaceholder() {
    val formula = "G A (y, _):w. even(y)"

    new FormulaParser(Conf.struct).parse(formula) match {
      case None => assert(false)
      case Some(formula) => assertEquals(Globally(Forall(Uop("w", Vector(Var("y"), Var("_"))),
        Iop("even", Vector(Var("y")), false))), formula)
    }
  }

  @Test
  def testParseVarWrongPlaceholder() {
    val formula = "A (y):w. even(_)"

    new FormulaParser(Conf.struct).parse(formula) match {
      case None => assert(true)
      case Some(formula) => assert(false)
    }
  }
}