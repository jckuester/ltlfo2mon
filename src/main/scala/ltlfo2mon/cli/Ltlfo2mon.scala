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
package ltlfo2mon.cli

import ltlfo2mon.util._
import ltlfo2mon.monitor._
import ltlfo2mon.util.parsing._
import scala.collection._
import ltlfo2mon.datatype._
import ltlfo2mon.Conf
import scala.io.Source
import java.io.FileNotFoundException

object Ltlfo2mon {
  
  val cmdParser = new scopt.immutable.OptionParser[Config]("ltlfo2mon", "v1.0 beta") { 
    def options = Seq(
        arg("<ltlfo-formula>", "LTLFO formula.") { (v: String, c: Config) => c.copy(formula = v) }, // Syntax: p | q | \u03C6 & \u03C8 | \u03C6 || \u03C8 | X \u03C6 | \u03C6 U \u03C8 | G | F | -> | <-> | ")
        argOpt("<trace-on-stdin>", "Monitor reads a single trace from stdin.") { (v: String, c: Config) => c.copy() }, // Syntax: {<name>(<value>)} {} ..., e.g., {sms(123)} {}
        //opt("l", "look-up-table", "<file>", "Provide file with pre-computed SAs.") { (v: String, c: Config) => c.copy(saCacheFile = v) },        
        opt("o", "output", "<file>", "Write montior's statistics (size, number of submonitors, etc.) to file.") { (v: String, c: Config) => c.copy(outputFile = v) },
        flag("p", "progression", "Use progression/formula-rewriting as monitor; default is SA-based monitor.") { (c: Config) => c.copy(progression = true) },
        flag("v", "verbose", "Show monitor's statistics (size, number of submonitors, etc.) after each step.") { (c: Config) => c.copy(verbose = true) },
        flag("vv", "verbose level 2", "Show monitor's inner-state after each step.") { (c: Config) => c.copy(verbose = true, verbose2 = true) }
        ) }
  
  def main(args: Array[String]): Unit = {
    cmdParser.parse(args, Config()) map { config =>
      // set verbose mode
      Conf.verbose = config.verbose
      Conf.verbose2 = config.verbose2
      
      val formulaParser = new FormulaParser(Conf.struct)
      val traceParser = new TraceParser(Conf.struct)

      formulaParser.parse(config.formula) match {
        case None => exit
        case Some(formula) => {
          try{
            val traces = Source.stdin.getLines()
            val trace = traces.next            
            if (traces.hasNext) {
              println("More than one trace in file."); exit
            } else {
              traceParser.parse(trace) match {
                case None => exit
                case Some(trace) => {
                  if (config.progression) {
                    val progression = new Progression(formula, Conf.struct)
                    val output = progression.process(trace, config.outputFile)
                    if (!Conf.verbose)
                      println("Result: " + output._1 + " after " + output._2 + " events."); return
                  } else {
                    if (config.saCacheFile == "") {
                      // read or create look-up table
                      //println("INFO: No look-up table provided; SAs will be pre-computed.")
                      SA.setSAcache(formula)
                    } else {
                      SA.setSAcacheFromFile(config.saCacheFile)
                    }
                    val saBasedMonitor = new SAbasedMonitor(formula, Conf.struct)
                    val output = saBasedMonitor.process(trace, config.outputFile)
                    if (!Conf.verbose)
                      println("Result: " + output._1 + " after " + output._2 + " events."); return
                  }
                }
              }
            }
          } catch {
            case e: FileNotFoundException => println("No or wrong trace-file given."); exit
          }
        }
      }     
    } getOrElse {
      // arguments are bad, usage message will have been displayed
    }
  }
  
/*
 * config for scopt
 */
case class Config(formula: String = "",
  outputFile: String = "", progression: Boolean = false, saCacheFile: String = "", verbose: Boolean = false, verbose2: Boolean = false)
  
}

