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
package ltlfo2mon.datatype

import ltlfo2mon.datatype.Types._
import ltlfo2mon.monitor._
import scala.collection._

/**
 * Positive Boolean formula
 */
trait BooleanFormula {
  override def toString: String
  def process(event: Event, executedConjs: mutable.HashMap[Int,Boolean3]): Boolean3  
  def getMonitors: Set[_ <: IMonitor]
  def length: Int
  def monSize(conjHashSet: mutable.HashSet[Conj]): Int
}

/*
 * Conjunction of sub-monitors
 */
class Conj(var elems: Set[BooleanFormula] = Set[BooleanFormula](), isBf: Boolean = false) extends BooleanFormula {  
  override def toString = if(elems.isEmpty) "\u22A4" else elems.mkString(" \u2227 ") 
  
  def process(event: Event, executedConjs: mutable.HashMap[Int,Boolean3]): Boolean3 = {
    executedConjs.get(this.hashCode) match {
      case None => {
        elems = elems.flatMap(elem => 
        elem.process(event, executedConjs) match {
          // if one element is false return false
          case Bottom() => { executedConjs.put(this.hashCode,Bottom()); return Bottom() }
          // delete elements that are true
          case Top() => None
          // inconclusive elements ( Top && Unknown == Unknown)
          case Unknown() => Some(elem)
        })      
        if(elems.isEmpty) {
          executedConjs.put(this.hashCode,Top())
          return Top()
        } else {
          executedConjs.put(this.hashCode,Unknown())
          return Unknown()
        }
      }
      case Some(result) => {
        return result
      }              
    }     
  }
  
  def isEmpty = elems.isEmpty
  def length = elems.toSeq.map(_.length).sum + (elems.size-1)
  def monSize(conjHashSet: mutable.HashSet[Conj]): Int = {
    if(conjHashSet(this)) {      
      return 1
    } else {
      conjHashSet += this
      return elems.toSeq.map(e => e.monSize(conjHashSet) + {if(e.isInstanceOf[Conj] || e.isInstanceOf[Neg]) 0 else 1}).sum + {if(elems.size>0) elems.size-1 else 0}
    }
  }  
  def getMonitors = elems.map{_.getMonitors}.flatten
  override def hashCode = elems.hashCode
}

class Neg(elem: BooleanFormula) extends BooleanFormula {
  override def toString = "\u00AC" + "(" + elem.toString + ")"
  
  def process(event: Event, executedConjs: mutable.HashMap[Int,Boolean3]): Boolean3 = {
    elem.process(event, executedConjs) match {
      case Bottom() => return Top()
      case Top() => return Bottom()
      case Unknown() => return Unknown()
    }
  }
  
  def length = elem.length + 1
  def monSize(conjHashSet: mutable.HashSet[Conj]): Int = {
    elem.monSize(conjHashSet) + 1
  }
  def getMonitors = elem.getMonitors
  override def hashCode = ("\u00AC" + elem.hashCode).hashCode
}
