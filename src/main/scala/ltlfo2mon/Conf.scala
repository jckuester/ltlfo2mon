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
  struct.addUoperator("w") 
   
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
  struct.addIoperator("m", (args:Vector[Any]) => if((Conf.index+77) % (args(0).toString.toInt+80) == 0) true else false)
  struct.addIoperator("n", (args:Vector[Any]) => if((Conf.index+21) % (args(0).toString.toInt+80) == 0) true else false)
  struct.addIoperator("o", (args:Vector[Any]) => if((Conf.index+7) % (args(0).toString.toInt+80) == 0) true else false)
  struct.addIoperator("p", (args: Vector[Any]) => false, true) // rigid false
  struct.addIoperator("q", (args: Vector[Any]) => args(0).toString.toInt % 2 == 0) // even
    // r(x): r becomes true max. 0-x worlds later 
  struct.addIoperator("r", (args:Vector[Any]) => if(Conf.index % (args(0).toString.toInt+1) == 0) true else false) 
  struct.addIoperator("s", (args:Vector[Any]) => if(Conf.index % 20 == 0) true else false)
  struct.addIoperator("t", (args:Vector[Any]) => if(Conf.index % (args(0).toString.toInt+args(1).toString.toInt+1) == 0) true else false)
  struct.addIoperator("u", (args:Vector[Any]) => true, true) // rigid true

  /*
   * print options
   */
  var verbose = false
  var verbose2 = false    
  // count level 0 SAs as deterministic (work around -> implementation follows)
  val countDeterministic = true
}