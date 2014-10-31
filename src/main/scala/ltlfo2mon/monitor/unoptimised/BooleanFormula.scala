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

import scala.collection._
import ltlfo2mon.datatype._
import ltlfo2mon.datatype.Types._
import ltlfo2mon.monitor._

/**
 * Positive Boolean formula
 */
trait BooleanFormula {
  override def toString: String

  def process(event: Event, executedConjs: mutable.HashMap[Int, Boolean3]): Boolean3

  def getMonitors: Set[_ <: IMonitor]

  def length: Int

  def monSize(bfHashSet: mutable.HashSet[BooleanFormula]): Int
}

/*
 * Conjunction of sub-monitors
 */
class Conj(var elems: Set[BooleanFormula] = Set[BooleanFormula]()) extends BooleanFormula {
  override def toString = if (elems.isEmpty) "\u22A4" else elems.mkString(" \u2227 ")

  def process(event: Event, executedConjs: mutable.HashMap[Int, Boolean3]): Boolean3 = {
    executedConjs.get(this.hashCode) match {
      case None =>
        elems = elems.flatMap(elem =>
          elem.process(event, executedConjs) match {
            // if one element is false return false
            case Bottom =>
              executedConjs.put(this.hashCode, Bottom)
              return Bottom
            // delete elements that are true
            case Top => None
            // inconclusive elements ( Top && Unknown == Unknown)
            case Unknown => Some(elem)
          })
        if (elems.isEmpty) {
          executedConjs.put(this.hashCode, Top)
          Top
        } else {
          executedConjs.put(this.hashCode, Unknown)
          Unknown
        }
      case Some(result) =>
        result
    }
  }

  override def equals(that: Any): Boolean = {
    that.isInstanceOf[Conj] && (this.hashCode() == that.asInstanceOf[Conj].hashCode())
  }

  override def hashCode = elems.hashCode()

  def isEmpty = elems.isEmpty

  def length = elems.toSeq.map(_.length).sum + (elems.size - 1)

  def monSize(bfHashSet: mutable.HashSet[BooleanFormula]): Int = {
    if (bfHashSet(this)) {
      1
    } else {
      if(!this.isEmpty) {
        // do not store '\top'
        bfHashSet.add(this)
      }
      elems.toSeq.map(e => e.monSize(bfHashSet) + {
        // count 1 for reference to SA
        if (e.isInstanceOf[Conj] || e.isInstanceOf[Neg]) 0 else 1
      }).sum + {
        // count 'and' operators
        if (elems.size > 0) elems.size - 1 else 0
      }
    }
  }

  def getMonitors = elems.map {
    _.getMonitors
  }.flatten
}

class Neg(elem: BooleanFormula) extends BooleanFormula {
  override def toString = "\u00AC" + "(" + elem.toString + ")"

  def process(event: Event, executedConjs: mutable.HashMap[Int, Boolean3]): Boolean3 = {
    elem.process(event, executedConjs) match {
      case Bottom => Top
      case Top => Bottom
      case Unknown => Unknown
    }
  }

  def length = elem.length + 1

  def monSize(bfHashSet: mutable.HashSet[BooleanFormula]): Int = {
    if (bfHashSet(this)) {
      1
    } else {
      bfHashSet.add(this)
      elem.monSize(bfHashSet) + 1
    }
  }

  def getMonitors = elem.getMonitors

  override def equals(that: Any): Boolean = {
    that.isInstanceOf[Neg] && (this.hashCode() == that.asInstanceOf[Neg].hashCode())
  }

  override def hashCode = 27 * elem.hashCode
}
