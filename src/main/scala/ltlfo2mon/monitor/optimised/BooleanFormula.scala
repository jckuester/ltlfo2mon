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
import ltlfo2mon.monitor._
import scala.collection._

/**
 * Positive Boolean formula
 */
trait BooleanFormula {
  override def toString: String

  def process(event: Event): Boolean3

  def getMonitors: Set[_ <: IMonitor]

  def length: Int

  def monSize(bfHashSet: mutable.HashSet[BooleanFormula]): Int

  var result: Option[Boolean3] = None
}

object BooleanFormula {
  /*
   * wrapper that tries to find $bf in cache before and after processing, and adds it otherwise
   */
  def process(bf: BooleanFormula, event: Event, cachedBFormulae: mutable.HashSet[BooleanFormula]): (BooleanFormula, Boolean3) = {
    // check if bf is in cache before processing
    cachedBFormulae.find(_.equals(bf)) match {
      case None =>
        val result = bf.process(event)
        // check if bf is in cache after processing
        cachedBFormulae.find(_.equals(bf)) match {
          // if not in cache, return original (processed) bf and add to cache
          case None =>
            cachedBFormulae.add(bf)
            (bf, result)
          // if in cache, return bf from cache
          case Some(exBf) => (exBf, result)
        }
      case Some(exBf) =>
        (exBf, exBf.process(event))
    }
  }
}

/*
 * Conjunction of sub-monitors
 */
class Conj(cachedBFormulae: mutable.HashSet[BooleanFormula],
           var elems: Set[BooleanFormula] = Set[BooleanFormula]()) extends BooleanFormula {

  override def toString = if (elems.isEmpty) "\u22A4" else "[" + elems.mkString(" \u2227 ") + "]"

  def process(event: Event): Boolean3 = {
    if (result.isEmpty) {
      var tmpElems = Set[BooleanFormula]()

      elems.foreach(elem => {
        val (tmpElem, tmpResult) = BooleanFormula.process(elem, event, cachedBFormulae)
        tmpResult match {
          // if one element is false return false
          case Bottom =>
            result = Some(Bottom)
            return Bottom
          // delete elements that are true
          case Top =>
          // inconclusive elements ( Top && Unknown == Unknown)
          case Unknown => tmpElems += tmpElem
        }
      })
      elems = tmpElems

      if (elems.isEmpty) {
        result = Some(Top)
        Top
      } else {
        result = Some(Unknown)
        Unknown
      }
    } else {
      // get result from cache (already executed)
      result.get
    }
  }

  def isEmpty = elems.isEmpty

  def length = elems.toSeq.map(_.length).sum + (elems.size - 1)

  def monSize(bfHashSet: mutable.HashSet[BooleanFormula]): Int = {
    if (bfHashSet(this)) {
      // count '1' for storing a reference
      1
    } else {
      if(!this.isEmpty) {
        // do not store '\top'
        bfHashSet.add(this)
      }
      elems.toSeq.map(e => e.monSize(bfHashSet) + {
        // count '1' for reference to SA
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

  override def equals(that: Any): Boolean = {
    that.isInstanceOf[Conj] && (this.hashCode() == that.asInstanceOf[Conj].hashCode())
  }

  override def hashCode = elems.hashCode()
}

class Neg(cachedBFormulae: mutable.HashSet[BooleanFormula],
          var elem: BooleanFormula) extends BooleanFormula {

  override def toString = "\u00AC" + elem.toString

  def process(event: Event): Boolean3 = {
    if (result.isEmpty) {
      val (tmpElem, tmpResult) = BooleanFormula.process(elem, event, cachedBFormulae)
      elem = tmpElem
      tmpResult match {
        case Bottom =>
          result = Some(Top)
          Top
        case Top =>
          result = Some(Bottom)
          Bottom
        case Unknown =>
          result = Some(Unknown)
          Unknown
      }
    } else {
      // get result from cache (already executed)
      result.get
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