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
import scala.collection.mutable
import scala.collection.mutable.HashSet

case class Run(state: State, obls: Obligations) {
  override def toString = "(" + state + ", " + obls.mkString("[", ",", "]") + ")"

  // count 1 for the head state in run
  def monSize(bfHashSet: mutable.HashSet[BooleanFormula]) = obls.toSeq.map(_.monSize(bfHashSet)).sum + 1

  def length = {
    if(obls.size == 0) {
      1
    } else {
      obls.size
    }
  }
}