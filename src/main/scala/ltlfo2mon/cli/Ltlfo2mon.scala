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
package ltlfo2mon.cli

import java.io.FileNotFoundException

import ltlfo2mon.Conf
import ltlfo2mon.monitor._
import ltlfo2mon.util.parsing._

import scala.collection._
import scala.io.Source

object Ltlfo2mon {

  val cmdParser = new scopt.immutable.OptionParser[Config]("ltlfo2mon", "v1.3") {
    def options = Seq(
      arg("<LTLFO formula>", "LTLFO formula that gets monitored.") { (v: String, c: Config) => c.copy(formula = v)}, // Syntax: p | q | \u03C6 & \u03C8 | \u03C6 || \u03C8 | X \u03C6 | \u03C6 U \u03C8 | G | F | -> | <-> | ")
      argOpt("<trace>", "Monitor reads single trace from stdin.") { (v: String, c: Config) => c.copy()}, // Syntax: {<name>(<value>)} {} ..., e.g., {sms(123)} {}
      //opt("l", "look-up-table", "<file>", "Provide file with pre-computed SAs.") { (v: String, c: Config) => c.copy(saCacheFile = v) },
      opt("o", "output", "<file>", "Write monitor statistics (size, number of submonitors, etc.) to file.") { (v: String, c: Config) => c.copy(outputFile = v)},
      flag("p", "progression", "Use progression/formula rewriting as monitor.") { (c: Config) => c.copy(progression = true)},
      flag("sa", "sa-monitor", "Use deprecated SA-based monitor (default is optimized SA-based monitor, based on ltl3tools).") { (c: Config) => c.copy(sa = true)},
      flag("v", "verbose", "Show monitor's statistics (size, number of submonitors, etc.) after each step.") { (c: Config) => c.copy(verbose = true)},
      flag("vv", "verbose level 2", "Show monitor's inner-state after each step.") { (c: Config) => c.copy(verbose = true, verbose2 = true)}
    )
  }

  def main(args: Array[String]): Unit = {
    cmdParser.parse(args, Config()).fold {
      // arguments are bad, usage message will have been displayed
    } { config =>
      // set verbose mode
      Conf.verbose = config.verbose
      Conf.verbose2 = config.verbose2

      val formulaParser = new FormulaParser(Conf.struct)
      val traceParser = new TraceParser(Conf.struct)

      formulaParser.parse(config.formula) match {
        case None => sys.exit()
        case Some(formula) =>
          try {
            val traceFile = Source.stdin.getLines()
            val traceStr = traceFile.next()
            if (traceFile.hasNext) {
              println("More than one trace in file.")
              sys.exit()
            } else {
              traceParser.parse(traceStr) match {
                case None => sys.exit()
                case Some(trace) =>
                  /*
                   * Progression
                   */
                  if (config.progression) {
                    val progression = new Progression(formula, Conf.struct)
                    val output = progression.process(trace, config.outputFile)
                    if (!Conf.verbose)
                      println("Result Progression: " + output._1 + " after " + output._2 + " events.")
                    return

                  } else if (config.sa) {
                    /*
                     * naive SAbasedMonitor
                     */
                    if (config.saCacheFile == "") {
                      // read or create look-up table
                      //println("INFO: No look-up table provided; SAs will be pre-computed.")
                      unoptimised.SA.setLookupTable(formula)
                    } else {
                      unoptimised.SA.setLookupTableFromFile(config.saCacheFile)
                    }
                    val saBasedMonitor = new unoptimised.SAbasedMonitor(formula, Conf.struct)
                    val output = saBasedMonitor.process(trace, config.outputFile)
                    if (!Conf.verbose)
                      println("Result SA: " + output._1 + " after " + output._2 + " events.")
                    return
                  } else {
                    /*
                     * optimised SAbasedMonitor
                     */
                    if (config.saCacheFile == "") {
                      // read or create look-up table
                      //println("INFO: No look-up table provided; SAs will be pre-computed.")
                      optimised.SAbasedMonitor.setLookupTable(formula)
                    } else {
                      optimised.SAbasedMonitor.setLookupTableFromFile(config.saCacheFile)
                    }
                    val saBasedMonitor = new optimised.SAbasedMonitor(formula, Conf.struct)
                    val output = saBasedMonitor.process(trace, config.outputFile)
                    if (!Conf.verbose)
                      println("Result SA-opt: " + output._1 + " after " + output._2 + " events.")
                    return
                  }
              }
            }
          } catch {
            case e: FileNotFoundException =>
              println("No or wrong trace-file given.")
              sys.exit()
          }
      }
    }
  }

  /*
   * config for scopt
   */
  case class Config(formula: String = "",
                    outputFile: String = "", progression: Boolean = false, sa: Boolean = false, saCacheFile: String = "", verbose: Boolean = false, verbose2: Boolean = false)

}
