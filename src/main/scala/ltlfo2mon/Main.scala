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
package ltlfo2mon
import java.io.{File, FileOutputStream, PrintWriter}

import ltlfo2mon.datatype._
import ltlfo2mon.monitor.Progression
import ltlfo2mon.util.parsing._

/*
 * main method to monitor formulae and traces defined in Conf.scala
 * note: main method for command line tool (ltlfo2mon.jar) defined in cli/Ltlfo2mon
 */
object Main {

  val formulaParser = new FormulaParser(Conf.struct)
  val traceParser = new TraceParser(Conf.struct)

  def main(args: Array[String]) {
    /*
     * generate or read predefined traces
     */
    var traces: List[Trace] = List()
    if (Conf.traces.isEmpty) {
      println("no traces provided.")
      return
    } else {
      for (traceString <- Conf.traces) {
        traces = traceParser.parse(traceString).get :: traces
      }
    }

    /*
     * monitor each formula
     */

    // start index of new files wrt. already existing files 
    val numOfPrgrFiles = Option(new File(Conf.path).list).fold(0)(_.count(_.endsWith("_progression.dat")))
    val numOfSAFiles = Option(new File(Conf.path).list).fold(0)(_.count(_.endsWith("_SAbased.dat")))
    val numOfOptSAFiles = Option(new File(Conf.path).list).fold(0)(_.count(_.endsWith("_optSAbased.dat")))

    for ((formula, findex) <- Conf.formulae.zipWithIndex) {
      formulaParser.parse(formula) match {
        case None => println("Formula has incorrect syntax.")
        case Some(policy) =>
          //println("\n###########################")
          //println("\nPolicy: " + policy + "\n")
          //println("###########################")

          /*
           * formulae to file
           */
          val printWriter = new PrintWriter(new FileOutputStream(new File(Conf.path + "formulae.dat"), true))
          monitor.unoptimised.SA.setLookupTable(policy)
          monitor.optimised.SAbasedMonitor.setLookupTable(policy)
          //println("cache-size: " + monitor.unoptimised.SA.sizeLookupTable)
          //println("cache-size: " + monitor.optimised.SAbasedMonitor.sizeLookupTable)

          printWriter.println("$\\varphi_{" + (findex + numOfSAFiles + 1) + "}$" + "\t" + monitor.unoptimised.SA.lookupTable.map(_._2.toSeq.map(_.size)).flatten.sum + "\t" + "$" + policy.toTex + "$") //.replace("\\", "\\\\")
          printWriter.close()
          /*
           * monitor each trace
           */
          for (trace <- traces) {
            if (Conf.verbose) {
              println("\n###########################")
              println("\nTrace: " + trace.toString + "\n")
              println("###########################")
            }

            /*
             * progression monitor
             */

            val progressionOutput = new Progression(policy, Conf.struct).process(trace, Conf.path + "example" + {
              if (findex + numOfPrgrFiles + 1 < 10) 0
            } + (findex + numOfPrgrFiles + 1) + "_progression.dat")

            /*
             * SA-based monitor
             */
            //SA.storeSAcacheToFile("/tmp/out")
            //SA.setSAcacheFromFile("/tmp/out")
            //Printable.showSAcacheAsPdf

            val saOutput = new monitor.unoptimised.SAbasedMonitor(policy, Conf.struct).process(trace, Conf.path + "example" + {
              if (findex + numOfSAFiles + 1 < 10) 0
            } + (findex + numOfSAFiles + 1) + "_SAbased.dat")

            /*
             * optimised SA-based monitor
             */

            val optSaOutput = new monitor.optimised.SAbasedMonitor(policy, Conf.struct).process(trace, Conf.path + "example" + {
              if (findex + numOfOptSAFiles + 1 < 10) 0
            } + (findex + numOfOptSAFiles + 1) + "_optSAbased.dat")

            /*
             * Compare monitor results
             */
            assert(progressionOutput._1 == saOutput._1)
            assert(progressionOutput._2 == saOutput._2)
            assert(progressionOutput._2 == optSaOutput._2)
          }
      }
    }
  }
}