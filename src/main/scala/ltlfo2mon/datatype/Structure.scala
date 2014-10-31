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

import scala.collection._

/**
 * First-order structure
 * require: uOps, iOps, functs and consts must be disjoint
 */
class Structure {
  var uOps: mutable.HashSet[String] = mutable.HashSet[String]()
  var iOps: mutable.HashMap[String, (Vector[Any] => Boolean, Boolean)] = mutable.HashMap[String, (Vector[Any] => Boolean, Boolean)]()
  var functs: mutable.HashMap[String, Vector[Any] => Any] = mutable.HashMap[String, Vector[Any] => Any]()
  var consts: mutable.HashMap[String, Any] = mutable.HashMap[String, Any]()

  def addUoperator(name: String) = if (isFreeName(name)) uOps.add(name)

  def addIoperator(name: String, interpr: Vector[Any] => Boolean, isRigid: Boolean = false) = if (isFreeName(name)) iOps.put(name, (interpr, isRigid))

  def addFunct(name: String, interpr: Vector[Any] => Any) = if (isFreeName(name)) functs.put(name, interpr)

  def addConst(name: String, interpr: Any) = if (isFreeName(name)) consts.put(name, interpr)

  def isFreeName(name: String): Boolean = {
    if (!uOps(name) && !iOps.keySet(name) && !functs.keySet(name) && !consts.keySet(name)) {
      true
    } else {
      throw new Exception("uOps, iOps, functs and consts must be disjoint.")
    }
  }
}
