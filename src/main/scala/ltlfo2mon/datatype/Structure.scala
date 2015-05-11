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

import ltlfo2mon.util.parsing.FormulaParser

import scala.collection._

/**
 * First-order structure
 * require: uOps, iOps, functs and consts must be disjoint
 */
class Structure {
  var uOps: mutable.HashSet[String] = mutable.HashSet[String]()
  var iOps: mutable.HashMap[String, (Vector[Any] => Boolean, Boolean)] =
    mutable.HashMap[String, (Vector[Any] => Boolean, Boolean)]()
  var functs: mutable.HashMap[String, Vector[Any] => Any] = mutable.HashMap[String, Vector[Any] => Any]()
  var consts: mutable.HashMap[String, Any] = mutable.HashMap[String, Any]()
  var vars: mutable.HashSet[String] = mutable.HashSet[String]()

  def addUoperator(name: String, throwException: Boolean = true) =
    if (isNameFree(name, throwException) && isIdentifier(name, throwException)) uOps.add(name) else false

  def addIoperator(name: String, interpr: Vector[Any] => Boolean, isRigid: Boolean = false) =
    if (isNameFree(name) && isIdentifier(name)) iOps.put(name, (interpr, isRigid))

  def addFunct(name: String, interpr: Vector[Any] => Any) =
    if (isNameFree(name) && isIdentifier(name)) functs.put(name, interpr)

  def addConst(name: String, interpr: Any, throwException: Boolean = true) =
    if (isNameFree(name, throwException)) { consts.put(name, interpr); true } else false

  def addVar(name: String, throwException: Boolean = true) =
    if (isNameFree(name, throwException) && isIdentifier(name, throwException)) vars.add(name) else false


  /*
   * check if names are Java identifiers or reserved keywords
   */
  def isIdentifier(name: String, throwException: Boolean = true): Boolean = {
    if(name.matches("""[a-zA-Z_$][a-zA-Z\d_$]*""") && !new FormulaParser(this).lexical.reserved(name)) {
      true
    } else {
      if(throwException) {
        throw new IllegalArgumentException("U-Op, I-Op, function, constant and variable names " +
          "must be Java identifiers.")
      } else {
        false
      }
    }
  }

  def isNameFree(name: String, throwException: Boolean = true): Boolean = {
    if(!(uOps(name) || iOps.keySet(name) || functs.keySet(name) || consts.keySet(name) || vars(name))) {
      true
    } else {
      if(throwException) {
        throw new IllegalArgumentException("U-Op, I-Op, function, constant and variable names must be disjoint.")
      } else {
        false
      }
    }
  }
}