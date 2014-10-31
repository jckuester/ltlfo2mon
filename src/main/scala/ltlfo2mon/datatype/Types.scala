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

import ltlfo2mon.monitor.unoptimised.Conj

import scala.collection._

object Types {
  type Valuation = immutable.HashMap[String, Any]
  type Event = Set[Action]
  type Action = Pair[String, Vector[Any]]
  //TODO
  type Obligations = mutable.Buffer[Obligation]
  type Obligation = Conj
  type Formulae = Set[Formula]
}
