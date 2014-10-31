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

import ltlfo2mon.datatype.Types._
import ltlfo2mon.datatype._

@serializable
class State(var name: String, var formulae: Formulae = Set[Formula](), var isInitial: Boolean = false, var ltl3type: Boolean3 = Unknown) {
  override def toString = name

  /*
   * transitions
   */
  var targetStates: Set[State] = Set[State]()

  def interpret(struct: Structure, event: Event, v: Valuation): Boolean = {
    var ret: Boolean = true
    formulae.foreach {
      case False => ret = false
      // interpret p(t)'s
      case uop: Uop => ret &= uop.interpret(struct, event, v)
      // interpret r(t)'s
      case iop: Iop => ret &= iop.interpret(struct, v)
      case Not(phi) =>
        phi match {
          // evaluate !p(t)'s
          case uop: Uop => ret &= !uop.interpret(struct, event, v)
          // evaluate !r(t)'s
          case iop: Iop => ret &= !iop.interpret(struct, v)
          case _ =>
        }
      case _ =>
    }
    ret
  }

  def size = {
    if (formulae.isEmpty) 1 else formulae.size
  } + targetStates.size

  /*
   * two states are equal if their names and formulae equal
   */
  override def equals(that: Any): Boolean = {
    that.isInstanceOf[State] && (this.hashCode() == that.asInstanceOf[State].hashCode())
  }

  override def hashCode = (name + formulae.hashCode).hashCode
}