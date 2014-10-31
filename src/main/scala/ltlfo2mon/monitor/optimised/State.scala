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
package ltlfo2mon.monitor.optimised

import ltlfo2mon.datatype._

case class State(name: String, var isInitial: Boolean = false, var ltl3type: Boolean3 = Unknown, var transitions: Set[Transition] = Set[Transition]()) {
  override def toString = name
  override def hashCode = name.hashCode()
  
  // count '1' per state and per empty transition, and '1' per (quantified) atom of transition
  def size = transitions.toSeq.map(t => if(t.guard.isEmpty) 1 else t.guard.size).sum + 1
}