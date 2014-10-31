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

import java.io._

import ltlfo2mon.Conf
import ltlfo2mon.datatype.Types._
import ltlfo2mon.datatype._
import ltlfo2mon.monitor.IMonitor
import ltlfo2mon.util.Printable._

import scala.collection._
import scala.util.control.Breaks._

/*
 * Algorithm M
 */
class SAbasedMonitor(phi: Formula, struct: Structure, v: Valuation = immutable.HashMap[String, Any]()) extends IMonitor(phi) with BooleanFormula {
  lazy val name = "SA-based monitor"

  override def toString = "M: " + phi + {
    if (v.isEmpty) "" else v.mkString("{", ", ", "}")
  }

  /*
   * M1: Create two instances of Algorithm T.
   */
  private val sa = new SA(phi, struct, v)
  private val saNeg = new SA(Not(phi), struct, v)

  /*
   * M2 & M3: forward event to SA_phi and SA_notphi, and communicate verdict.
   */
  def process(event: Event, executedConjs: mutable.HashMap[Int, Boolean3]): Boolean3 = {
    val createdConjs: mutable.HashMap[String, Conj] = new mutable.HashMap[String, Conj]()
    val newExecutedConjs = new mutable.HashMap[Int, Boolean3]()

    if (sa.process(event, createdConjs, newExecutedConjs)) Bottom
    else if (saNeg.process(event, createdConjs, newExecutedConjs)) Top
    else Unknown
  }

  /*
   * processes a complete trace
   */
  def process(trace: Trace, outputFile: String): (Boolean3, Int) = {

    var result: Boolean3 = null
    var sizeOfEvents: List[Int] = List()
    var size: List[Int] = List()
    var numOfSubmonitors: List[Int] = List()
    var maxNumOfRuns: List[Int] = List()
    var maxNumOfObls: List[Int] = List()
    var maxOblLength: List[Int] = List()

    /*
     * experiments file output header
     */
    var printWriter: Option[PrintWriter] = None
    if (outputFile != "") {
      try {
        printWriter = Some(new PrintWriter(new FileOutputStream(new File(outputFile), true)))
      } catch {
        case e: FileNotFoundException => println(e.getMessage); sys.exit()
      }
    }

    printWriter match {
      case None =>
      case Some(writer) =>
        writer.println("# " + this.name)
        writer.println("# Formula: " + this.phi)
        //writer.println("#Trace: " + trace)
        writer.println("# Trace-length: " + trace.length)
        writer.println("# Size-lookup-table: " + SA.sizeLookupTable)
        writer.println("#")
        writer.println("# Ev-sz = Event-size, M-sz = Mon-size, Exec-ms = Execution time in ms, #mons = Total num mons")
        writer.println("# M#runs / M#rs = max number of runs, MLruns / MLrs = max length runs, AV#runs / AV#rs = avg number of runs, AVLruns / AVLrs = avg length of runs")
        writer.println("#")
        writer.print("#Time" + "\t" + "Ev-sz" + "\t" + "M-sz" + "\t" + "Exec-ms" + "\t" + "#mons" +
          "\t" + "M#runs" + "\t" + "MLruns" + "\t" + "MLobl" +
          "\t" + "AV#runs" + "\t" + "AVLruns" + "\t" + "AVLobl")
        for (level <- 0 to this.level) {
          writer.print("\t" + "L" + level + "#mons" +
            "\t" + "L" + level + "M#rs" + "\t" + "L" + level + "MLrs" + "\t" + "L" + level + "MLobl" +
            "\t" + "L" + level + "AV#rs" + "\t" + "L" + level + "AVLrs" + "\t" + "L" + level + "AVLo")
        }
        writer.println()
    }

    breakable {
      for ((event, i) <- trace.events.zipWithIndex) {
        val index = i + 1
        Conf.index = index

        /*
         * process event
         */
        var start, end: Long = 0
        start = System.currentTimeMillis()
        result = this.process(event, new mutable.HashMap[Int, Boolean3]())
        end = System.currentTimeMillis()

        val monSize = this.size

        sizeOfEvents = event.size :: sizeOfEvents
        size = monSize :: size
        numOfSubmonitors = this.numOfMonitors() :: numOfSubmonitors
        maxNumOfRuns = this.maxNumOfRuns() :: maxNumOfRuns
        maxNumOfObls = this.maxLengthOfRuns() :: maxNumOfObls
        maxOblLength = this.maxOblLength() :: maxOblLength

        /*
         * experiments file output
         */
        printWriter match {
          case None =>
          case Some(writer) =>
            writer.print(index + "\t" + event.size + "\t" + monSize + "\t" + (end - start) +
              "\t" + this.numOfMonitors() + "\t" + this.maxNumOfRuns() + "\t" + this.maxLengthOfRuns() + "\t" + this.maxOblLength() +
              "\t" + this.avgNumOfRuns() + "\t" + this.avgLengthOfRuns() + "\t" + this.avgOblLength())
            for (level <- 0 to this.level) {
              writer.print("\t" + this.numOfMonitors(level) +
                "\t" + this.maxNumOfRuns(level) + "\t" + this.maxLengthOfRuns(level) + "\t" + this.maxOblLength(level) +
                "\t" + this.avgNumOfRuns(level) + "\t" + this.avgLengthOfRuns(level) + "\t" + this.avgOblLength(level))
            }
            writer.println()
        }

        if (Conf.verbose2) {
          println("\n### TIME " + index + " ###")
          println("Monitor size: " + monSize)
          println("Number of monitors: " + this.numOfMonitors())
          println("Max number of runs: " + this.maxNumOfRuns())
          println("Max length of runs: " + this.maxLengthOfRuns())
          println("Max length of obligation: " + this.maxOblLength())
          println("Event size: " + event.size)

          for (level <- this.level to 0 by -1) {
            val indent = this.level - level + 1
            val levelMons = this.getMonitors(level)
            println("\n" + "\t" * indent + "L" + level + " Monitors: " + levelMons.size)
            println("\t" * indent + "###")
            levelMons.foreach(m => println("\t" * indent + m.toString + " " + m.hashCode
              + "\n" + "\t" * (indent + 1) + m.sa.toString + ": " + m.sa.buffer.getOrElse(mutable.Buffer()).mkString("{", ",", "}")
              + "\n" + "\t" * (indent + 1) + m.saNeg.toString + ": " + m.saNeg.buffer.getOrElse(mutable.Buffer()).mkString("{", ",", "}")))
          }
        }


        result match {
          case Top | Bottom => break()
          case Unknown =>
        }
      }
    }

    /*
     * print final results
     */
    if (Conf.verbose) {
      if (Conf.verbose2) println("\n\n### FINAL RESULT ###")
      else println("### FINAL RESULT ###")
      println(this.name)
      println("Formula: " + this.phi)
      println("Result: " + result + " after " + size.length + " events.")
      println("Trace length: " + trace.length + "\n")

      val maxDigits = size.reverse.map(e => e.toString.length).max
      println("Monitor size:\t\t\t\t" + size.reverse.map(e => {
        " " * (maxDigits - e.toString.length)
      } + e).mkString(",\t"))
      println("Number of monitors:\t\t\t" + numOfSubmonitors.reverse.map(e => {
        " " * (maxDigits - e.toString.length)
      } + e).mkString(",\t"))
      println("Max number of runs:\t\t\t" + maxNumOfRuns.reverse.map(e => {
        " " * (maxDigits - e.toString.length)
      } + e).mkString(",\t"))
      println("Max length of runs:\t\t\t" + maxNumOfObls.reverse.map(e => {
        " " * (maxDigits - e.toString.length)
      } + e).mkString(",\t"))
      println("Max length of obligations:\t" + maxOblLength.reverse.map(e => {
        " " * (maxDigits - e.toString.length)
      } + e).mkString(",\t"))
      println("Size of events:\t\t\t\t" + sizeOfEvents.reverse.map(e => {
        " " * (maxDigits - e.toString.length)
      } + e).mkString(",\t"))
    }
    printWriter match {
      case None =>
      case Some(writer) =>
        writer.println()
        writer.close()
    }

    (result, size.length)
  }

  /*
   * log stuff
   */
  def level = this.phi.level

  def length = 1

  def size = monSize()

  def monSize(bfHashSet: mutable.HashSet[BooleanFormula] = new mutable.HashSet[BooleanFormula]()): Int = {
    if (bfHashSet(this)) {
      0
    } else {
      bfHashSet.add(this)
      this.sa.monSize(bfHashSet) + this.saNeg.monSize(bfHashSet)
    }
  }

  def numOfMonitors(level: Int = -1): Int = getMonitors(level).size

  def maxNumOfRuns(level: Int = -1) = getMonitors(level).map(m => m.sa.numOfRuns + m.saNeg.numOfRuns).reduceOption(_ max _).getOrElse(0)

  def avgNumOfRuns(level: Int = -1) = getMonitors(level).map(m => m.sa.numOfRuns + m.saNeg.numOfRuns).avg

  def maxLengthOfRuns(level: Int = -1) = getMonitors(level).toSeq.map(m => math.max(m.sa.maxLengthOfRuns, m.saNeg.maxLengthOfRuns)).reduceOption(_ max _).getOrElse(0)

  def avgLengthOfRuns(level: Int = -1) = getMonitors(level).toSeq.map(m => Set(m.sa.avgLengthOfRuns, m.saNeg.avgLengthOfRuns).avg).avg

  def maxOblLength(level: Int = -1) = getMonitors(level).toSeq.map(m => math.max(m.sa.maxOblLength, m.saNeg.maxOblLength)).reduceOption(_ max _).getOrElse(0)

  def avgOblLength(level: Int = -1) = getMonitors(level).toSeq.map(m => Set(m.sa.avgOblLength, m.saNeg.avgOblLength).avg).avg

  def getMonitors: Set[SAbasedMonitor] = sa.getMonitors ++ saNeg.getMonitors + this

  def getMonitors(level: Int = -1): Set[SAbasedMonitor] = if (level == -1) this.getMonitors else this.getMonitors.filter(_.level == level)

  override def equals(that: Any): Boolean = {
    that.isInstanceOf[SAbasedMonitor] && (this.hashCode() == that.asInstanceOf[SAbasedMonitor].hashCode())
  }

  override def hashCode = phi.hashCode() + v.hashCode() + sa.hashCode + saNeg.hashCode
}