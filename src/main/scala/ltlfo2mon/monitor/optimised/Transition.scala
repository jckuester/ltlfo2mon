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

case class Transition(targetState: State, guard: Formulae) {
  override def toString = "~" + guard.mkString("{", ", ", "}") + "~>" + targetState

  def interpret(struct: Structure, event: Event, v: Valuation): Boolean = {
    var ret: Boolean = true
    guard.foreach {
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
}