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
package ltlfo2mon.monitor

import java.io._

import ltlfo2mon.Conf
import ltlfo2mon.datatype.Types._
import ltlfo2mon.datatype._

import scala.collection._
import scala.util.control.Breaks._

/*
 * Monitor based on formula rewriting
 */
class Progression(phi: Formula, struct: Structure) extends IMonitor(phi) {
  lazy val name = "Progression"

  override def toString = rewrite.toString

  /*
   *  rewritten formula that has to hold in the future
   */
  var rewrite: Formula = phi

  /*
   * progression function
   */
  private def prg(phi: Formula, event: Event, v: Valuation): Formula = {
    phi match {
      case True => True
      case False => False
      case uop: Uop => if (uop.interpret(struct, event, v)) True else False
      case iop: Iop => if (iop.interpret(struct, v)) True else False
      case Not(psi) => Not(prg(psi, event, v))
      case And(psi, chi) => And(prg(psi, event, v), prg(chi, event, v)).eval
      case Or(psi, chi) => Or(prg(psi, event, v), prg(chi, event, v)).eval
      case Next(psi) => psi
      case until: Until => Or(prg(until.psi, event, v), And(prg(until.phi, event, v), until).eval).eval
      case Forall(p, psi) =>
        var elems: immutable.Set[(Formula, Valuation)] = immutable.Set[(Formula, Valuation)]()
        for (action <- event if action._1 == p.name) {
          var vNew: Valuation = v
          for ((variable, i) <- p.args.zipWithIndex) {
            vNew += ((variable.name, action._2(i)))
          }
          elems += ((psi, vNew))
        }
        prg(ForallConj(elems), event, v)
      case ForallConj(elems) =>
        ForallConj(elems.map { case (f, valuation) => (prg(f, event, valuation), valuation)}).eval
      case globally: Globally => And(prg(globally.phi, event, v), globally).eval
      case eventually: Eventually => Or(prg(eventually.phi, event, v), eventually).eval
    }
  }

  def process(event: Event): Boolean3 = {
    rewrite = prg(rewrite, event, new Valuation)

    rewrite match {
      case True => Top
      case False => Bottom
      case _ => Unknown
    }
  }

  /*
   * processes a complete trace
   */
  def process(trace: Trace, outputFile: String): (Boolean3, Int) = {
    var result: Boolean3 = null
    var sizeOfEvents: List[Int] = List()
    var size: List[Int] = List()
    var execTimes: List[Long] = List()

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
        writer.println("#" + this.name)
        writer.println("#Formula: " + this.phi)
        writer.println("#Trace: " + trace)
        writer.println("#Trace-length: " + trace.length)
        writer.println("#")
        writer.println("#Time" + "\t" + "Ev-sz" + "\t" + "F-len" + "\t" + "Exec-time-in-ms")
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
        result = this.process(event)
        end = System.currentTimeMillis()

        sizeOfEvents = event.size :: sizeOfEvents
        size = this.size :: size
        execTimes = (end - start) :: execTimes

        /*
         * experiments file output
         */
        printWriter match {
          case None =>
          case Some(writer) =>
            writer.println(index + "\t" + event.size + "\t" + this.size + "\t" + (end - start))
        }

        if (Conf.verbose2) {
          println("\n### TIME " + index + " ###")
          println("Rewritten formula: " + this.rewrite)
          println("Formula length: " + this.size)
          println("Execution time: " + (end - start))
          println("Event size: " + event.size)
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
      println("Trace length: " + size.length + '\n')
      println("Formula length: " + size.reverse.mkString(", "))
      println("Execution times: " + execTimes.reverse.mkString(", "))
      println("Size of events: " + sizeOfEvents.reverse.mkString(", "))
    }
    printWriter match {
      case None =>
      case Some(writer: PrintWriter) =>
        writer.println()
        writer.close()
    }

    (result, size.length)
  }

  def size = rewrite.length
}
