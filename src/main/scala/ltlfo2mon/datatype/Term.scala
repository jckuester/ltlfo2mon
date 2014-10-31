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

import ltlfo2mon.datatype.Types._

sealed trait Term {
  val name: String
  /*
   * Interprets a ground term
   * @return: None if valuations for variables are missing
   */
  def interpret(struct: Structure, v: Valuation): Option[Any]
}

case class Var(name: String) extends Term {
  override def toString = name

  def interpret(struct: Structure, v: Valuation) = v.get(name)
}

case class Funct(name: String, args: Vector[Term]) extends Term {
  def interpret(struct: Structure, v: Valuation) = struct.functs.get(name) match {
    case None => None
    case Some(funct) => Some(funct(args.map(_.interpret(struct, v)))) // TODO what if _.interpret returns None? 
  }
}

case class Const(name: String) extends Term {
  override def toString = name

  def interpret(struct: Structure, v: Valuation) = struct.consts.get(name)
}