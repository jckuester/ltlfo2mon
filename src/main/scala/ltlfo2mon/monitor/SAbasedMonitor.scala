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
package ltlfo2mon.monitor

import ltlfo2mon.datatype.Formula
import ltlfo2mon.datatype._
import ltlfo2mon.datatype.Types._
import ltlfo2mon.Conf
import ltlfo2mon.util.Printable._
import scala.collection._
import scala.io.Source
import scala.collection.mutable.Buffer
import java.io._
import scala.util.Random
import scala.collection.mutable.HashSet
import scala.util.control.Breaks._


/*
 * Algorithm M
 */
class SAbasedMonitor(phi: Formula, struct: Structure, v: Valuation = immutable.HashMap[String, Any]()) extends IMonitor(phi) with BooleanFormula {
  lazy val name = "SA-based monitor"
  override def toString = "M: " + phi + { if(v.isEmpty) "" else v.mkString("{", ", ", "}") }  
   
  /*
   * M1: Create two instances of Algorithm T.
   */
  private val sa = new SA(phi, struct, v)
  private val saNeg = new SA(Not(phi), struct, v)
  
  /*
   * M2 & M3: forward event to SA_phi and SA_notphi, and communicate verdict.
   */  
  def process(event: Event, executedConjs: mutable.HashMap[Int, Boolean3]): Boolean3 = {
    val createdConjs: mutable.HashMap[String, Conj] = new mutable.HashMap[String,Conj]()
    val newExecutedConjs = new mutable.HashMap[Int,Boolean3]()     
    
    if(sa.process(event, createdConjs, newExecutedConjs) == true) return Bottom()
    else if(saNeg.process(event, createdConjs, newExecutedConjs) == true) return Top()
    else return Unknown()
  }
  
  /*
   * processes a complete trace
   */  
  def process(trace: Trace, outputFile: String): (Boolean3,Int) = {
    
    var result: Boolean3 = null    
    var sizeOfEvents: List[Int] = List()
    var size: List[Int] = List()
    var numOfSubmonitors: List[Int] = List()
    var numOfSubmonitorsPerLevel: mutable.Map[Int,List[Int]] = mutable.Map[Int,List[Int]]()
    for(level <- 0 to this.level-1) {
      numOfSubmonitorsPerLevel += ((level, List()))
    }
    var maxNumOfRuns: List[Int] = List()
    var maxNumOfObls: List[Int] = List()
    var maxOblLength: List[Int] = List()
    
    /*
     * experiments file output header
     */
    var printWriter: Option[PrintWriter] = None
    if(outputFile != "") {
      try{
        printWriter = Some(new PrintWriter(new FileOutputStream(new File(outputFile),true)))
      } catch {
          case e: FileNotFoundException => println(e.getMessage()); exit      
      }
    } 
      
    printWriter match {
      case None => 
      case Some(printWriter) => {
        printWriter.println("#" + this.name)
        printWriter.println("#Formula: " + this.phi)
        printWriter.println("#Trace: " + trace)
        printWriter.println("#Trace-length: " + trace.length)
        printWriter.println("#")
        printWriter.println("#Index" + "\t" + "Event-size" + "\t" + "Size" + "\t" + "Num-of-submonitors" +
            "\t" + "Max-num-of-runs" + "\t" + "Max-num-of-obls" + "\t" + "Max-obl-length")  
      }
    }
    
    breakable{    
      for((event, i) <- trace.events.zipWithIndex) {
        val index = i+1
        Conf.index = index
        result = this.process(event, new mutable.HashMap[Int,Boolean3]())    
        
        sizeOfEvents = event.size :: sizeOfEvents
        size = this.size :: size
        numOfSubmonitors = this.numOfSubmonitors :: numOfSubmonitors
        for(level <- 0 to this.level -1) {
          val levelList = numOfSubmonitorsPerLevel.get(level).get
          numOfSubmonitorsPerLevel.update(level, this.numOfSubmonitorsPerLevel.get(level).get :: levelList)
        }    
        maxNumOfRuns = this.maxNumOfRuns :: maxNumOfRuns
        maxNumOfObls = this.maxNumOfObls :: maxNumOfObls
        maxOblLength = this.maxOblLength :: maxOblLength
        
        /*
         * experiments file output
         */
        printWriter match {
          case None => 
          case Some(printWriter) => {
             printWriter.println(index + "\t" + event.size + "\t" + this.size + "\t" + this.numOfSubmonitors +
                 "\t" + this.maxNumOfRuns + "\t" + this.maxNumOfObls + "\t" + this.maxOblLength)
          }
        }
  
        if(Conf.verbose2) {        
          println("\n### TIME " + index + " ###")
          println("Monitor size: " + this.size) 
          println("Number of sub-monitors: " + this.numOfSubmonitors)
          println("Max-number of runs: " + this.maxNumOfRuns)
          println("Max-num of obligations: " + this.maxNumOfObls)
          println("Max-length of obligation: " + this.maxOblLength)
          println("Event size: " + event.size)  
          
          for(level <- this.level to 0 by -1) {
            val indent = (this.level-level+1)
            val levelMons = this.getMonitors.filter(_.level == level)
                println("\n" + "\t"*indent + "L" + level + " Monitors: " + levelMons.size)
                println("\t"*indent + "###")
                levelMons.foreach(m => println("\t"*indent + m.toString
                    + "\n" + "\t"*(indent+1) + m.sa.toString + ": " + m.sa.buffer.getOrElse(Buffer()).mkString("{", ",", "}")
                    + "\n" + "\t"*(indent+1) + m.saNeg.toString + ": " + m.saNeg.buffer.getOrElse(Buffer()).mkString("{", ",", "}")))
          }         
        }        
        
        
        result match {
          case Top() | Bottom() => break
          case Unknown() =>
        }      
      }
    }
    
    /*
     * experiments file output
     */
    //printWriter.println("\n")
    //printWriter.println("#Index" + "\t" + "Level" + "\t" + "Num-of-submonitors")
    //printWriter.println("#")
    //for(level <- 0 to this.level-1) {
    //  for((numOfSubmons,index) <- numOfSubmonitorsPerLevel.get(level).get.zipWithIndex) {
    //    printWriter.println(index + "\t" + level + "\t" + numOfSubmons)  
    //  }      
    //  printWriter.println()
    //}
    
   
    
    /*
     * print final results
     */
    
    if(Conf.verbose) {  
      if(Conf.verbose2) println("\n\n### FINAL RESULT ###")
      else println("### FINAL RESULT ###") 
      println(this.name)
      println("Formula: " + this.phi)
      println("Result: " + result + " after " + size.length + " events.")
      println("Trace length: " + trace.length + "\n")
     
      println("Monitor size: " + size.reverse.mkString(", "))
      println("Number of sub-monitors: " + numOfSubmonitors.reverse.mkString(", "))
      for(level <- 0 to this.level-1) {
        println("Number of L" + level + " sub-monitors: " + 
            numOfSubmonitorsPerLevel.get(level).get.reverse.mkString(", "))
      }    
      println("Max-number of runs: " + maxNumOfRuns.reverse.mkString(", "))
      println("Max-num of obligations: " + maxNumOfObls.reverse.mkString(", "))
      println("Max-length of obligation: " + maxOblLength.reverse.mkString(", "))
      println("Size of events: " + sizeOfEvents.reverse.mkString(", "))
      /*
       * measure runtime of monitor
      
      val newMon = new SAbasedMonitor(this.phi, Config.struct)
      println("Runtime: " + time({for((event,index) <- trace.events.zipWithIndex) {
        Config.index = index
        newMon.process(event, new mutable.HashMap[Int,Boolean3]())
        }}) + "ms" + "\n")
       *    
       */
    }
     printWriter match {
      case None => 
      case Some(printWriter) => {
        printWriter.println()
        printWriter.close    
      }
    }

    return (result,size.length)
  }
  
  /*
   * log stuff
   */
  def level = this.phi.level
  def length = 1
  def size = monSize()
  def monSize(conjHashSet: HashSet[Conj] = new HashSet[Conj]()): Int = {
    val newConjHashSet = new HashSet[Conj]()
    // count level 0 SAs as deterministic (work around -> implementation follows)
    if(Conf.countDeterministic) {
      if(this.level != 0) {
        return this.sa.monSize(newConjHashSet) + this.saNeg.monSize(newConjHashSet)
      } else return 1
    } else return this.sa.monSize(newConjHashSet) + this.saNeg.monSize(newConjHashSet)
  }
  def maxNumOfRuns = getMonitors.map(m => math.max(m.sa.numOfRuns, m.saNeg.numOfRuns)).max
  def maxNumOfObls = getMonitors.toSeq.map(m => math.max(m.sa.maxNumOfObls, m.saNeg.maxNumOfObls)).max
  def maxOblLength = getMonitors.toSeq.map(m => math.max(m.sa.maxOblLength, m.saNeg.maxOblLength)).max
  def getMonitors: Set[SAbasedMonitor] = sa.getMonitors ++ saNeg.getMonitors + this
  def numOfSubmonitors: Int = getMonitors.size - 1
  def numOfSubmonitorsPerLevel: Map[Int, Int] = {
     var monsOnLevelMap = Map[Int,Int]()
      for(level <- 0 to this.level-1) {
        val monsOnLevel = this.getMonitors.filter(_.level == level)
        monsOnLevelMap = monsOnLevelMap + ((level, monsOnLevel.size))                
      }
     monsOnLevelMap
  }
  
  override def hashCode = (this.toString + sa.hashCode + saNeg.hashCode).hashCode
}
