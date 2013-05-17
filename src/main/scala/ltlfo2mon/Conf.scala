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
package ltlfo2mon

import datatype._
import scala.util.Random

object Conf {
  var index: Int = 1

  /**
   * First-order structure
   */
  var struct = new Structure()

  /**
   * U-operators
   */
  struct.addUoperator("p", () => Vector(Random.nextInt(100)))
  struct.addUoperator("q", () => Vector(Random.nextInt(100)))
   
  /**
   * Constants
   */
  struct.addConst("1", 1)
  struct.addConst("2", 2)
  struct.addConst("3", 3)
  struct.addConst("4", 4)
  struct.addConst("5", 5)
    
  /**
   * I-operators
   */
  struct.addIoperator("top", (args: Vector[Any]) => true, true)
  struct.addIoperator("bot", (args: Vector[Any]) => false, true)
  struct.addIoperator("leq", (args: Vector[Any]) => args(0).toString.toInt < args(1).toString.toInt, true)
  struct.addIoperator("eq", (args: Vector[Any]) => args(0).toString.toInt == args(1).toString.toInt, true)
  struct.addIoperator("even", (args: Vector[Any]) => args(0).toString.toInt % 2 == 0, true)
  struct.addIoperator("odd", (args: Vector[Any]) => args(0).toString.toInt % 2 != 0, true)
  struct.addIoperator("div4", (args:Vector[Any]) => args(0).toString.toInt % 4 == 0, true) 
   
  /*
   * print options
   */
  var verbose = false
  var verbose2 = false    
  val countDeterministic = false
}
