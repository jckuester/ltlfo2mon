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
package ltlfo2mon.util

import java.io._

import com.floreysoft.jmte.Engine
import ltlfo2mon.datatype._
import ltlfo2mon.monitor._
import ltlfo2mon.monitor.unoptimised._

import scala.collection._
import scala.io.Source
import scala.sys.process._


trait Printable {
  def toTex: String

  def toDot: String

  def toLBTstr(encodedAtomsHat: mutable.HashMap[Formula, Int]): String

  def toLTL3ToolsStr(encodedAtomsHat: mutable.HashMap[Formula, Int]): String
}

object Printable {
  def average[T](ts: Iterable[T])(implicit num: Numeric[T]) = {
    math.round(num.toDouble(ts.sum) / ts.size * 100).toDouble / 100
  }

  implicit def iterebleWithAvg[T: Numeric](data: Iterable[T]) = new {
    def avg = average(data)
  }

  implicit def any2Printable(any: Any): Printable = new Printable {

    def toDot = any match {
      case state: State => state.name.splitAt(1)._1 + "_" + state.name.splitAt(1)._2
      case qs: Set[State] => "digraph graphname {\n" +
        "\"\" [shape=none]\n" +
        // draw states
        qs.map { s: State => s.toDot + " [texlbl=\"$" +
          s.formulae.map { f => f.toTex}.mkString(", ") +
          "$\"];"
        }.mkString("\n") + "\n\n" +
        // draw inital states
        qs.filter(_.isInitial).map { s => "\"\" -> " + s.toDot + ";"}.mkString("\n") + "\n" +
        // draw transitions
        qs.map { s => s.targetStates.map { out => s.toDot + " -> " + out.toDot + ";"}.mkString("\n")}.mkString("\n") +
        "\n}"
    }

    def toTex = any match {
      case state: State => state.name + ": " + state.formulae.map(f => f.toTex).mkString(",")
      case states: Set[State] => states.map(state => state.toTex).mkString("$", ", ", "$")
    }


    /*
 * generates input string for LBT command line tool
 */
    def toLBTstr(encodedAtomsHat: mutable.HashMap[Formula, Int]): String = any match {
      case True => "t"
      case uop: Uop => "p" + encodedAtomsHat.get(uop).get.toString
      case iop: Iop => "p" + encodedAtomsHat.get(iop).get.toString
      case not: Not => "! " + not.phi.toLBTstr(encodedAtomsHat)
      case and: And => "& " + and.phi.toLBTstr(encodedAtomsHat) + " " + and.psi.toLBTstr(encodedAtomsHat)
      case or: Or => "| " + or.phi.toLBTstr(encodedAtomsHat) + " " + or.psi.toLBTstr(encodedAtomsHat)
      case next: Next => "X " + next.phi.toLBTstr(encodedAtomsHat)
      case until: Until => "U " + until.phi.toLBTstr(encodedAtomsHat) + " " + until.psi.toLBTstr(encodedAtomsHat)
      case forall: Forall => "p" + encodedAtomsHat.get(forall).get.toString
      case globally: Globally => "G " + globally.phi.toLBTstr(encodedAtomsHat)
      case eventually: Eventually => "F " + eventually.phi.toLBTstr(encodedAtomsHat)
      case _ => ""
    }

    /*
     * generates input string for ltl3tool 
     */
    def toLTL3ToolsStr(encodedAtomsHat: mutable.HashMap[Formula, Int]): String = any match {
      case True => "true"
      case uop: Uop => "p" + encodedAtomsHat.get(uop).get.toString
      case iop: Iop => "p" + encodedAtomsHat.get(iop).get.toString
      case not: Not => "! (" + not.phi.toLTL3ToolsStr(encodedAtomsHat) + ")"
      case and: And => "(" + and.phi.toLTL3ToolsStr(encodedAtomsHat) + ") && (" + and.psi.toLTL3ToolsStr(encodedAtomsHat) + ")"
      case or: Or => "(" + or.phi.toLTL3ToolsStr(encodedAtomsHat) + ") || (" + or.psi.toLTL3ToolsStr(encodedAtomsHat) + ")"
      case next: Next => "X (" + next.phi.toLTL3ToolsStr(encodedAtomsHat) + ")"
      case until: Until => "(" + until.phi.toLTL3ToolsStr(encodedAtomsHat) + ") U (" + until.psi.toLTL3ToolsStr(encodedAtomsHat) + ")"
      case forall: Forall => "p" + encodedAtomsHat.get(forall).get.toString
      case eventually: Eventually => "<> (" + eventually.phi.toLTL3ToolsStr(encodedAtomsHat) + ")"
      case globally: Globally => "[] (" + globally.phi.toLTL3ToolsStr(encodedAtomsHat) + ")"
      case _ => ""
    }
  }

  def unoptimisedSaCacheToTex(outputDir: String) = {
    // template engine
    val engine: Engine = new Engine()
    val sasModel: java.util.HashMap[String, Object] = new java.util.HashMap[String, Object]()
    try {
      val tmpl = Source.fromFile("./template.tex").mkString
      //Process("mkdir " + outputDir!!)
      for ((sa, i) <- SA.lookupTable.zipWithIndex) {
        val fileName = outputDir + "/sa" + i
        // dot -> ps
        Process(("echo " + sa._2.toDot) #| "dot -Tps" #> new File(fileName + ".ps") !!)
        // ps -> pdf
        Process("ps2pdf " + fileName + ".ps " + fileName + ".pdf" !!)
        // crop pdf
        Process("pdfcrop " + fileName + ".pdf " + fileName + ".pdf" !!)

        val saModel = new java.util.HashMap[String, Object]()
        saModel.put("pdf", "sa" + i + ".pdf")
        saModel.put("caption", sa._1.toTex)
        val stateModel = new java.util.HashMap[String, Object]()
        sa._2.foreach(state => stateModel.put(state.name, state.formulae.map(_.toTex).mkString(",")))
        saModel.put("states", stateModel)
        sasModel.put("sa" + i, saModel)
      }
      val model: java.util.HashMap[String, Object] = new java.util.HashMap[String, Object]()
      model.put("sas", sasModel)
      // write template as output
      Process(("echo " + engine.transform(tmpl, model)) #> new File(outputDir + "/report.tex") !!)
      println("INFO: Report written to directory " + outputDir + ".")
    } catch {
      case e: FileNotFoundException => println(e.getMessage); sys.exit()
      case e: RuntimeException => println(""); sys.exit()
    }
  }

  def optimisedSaCacheToTex(outputDir: String) = {
    // template engine
    val engine: Engine = new Engine()
    val sasModel: java.util.HashMap[String, Object] = new java.util.HashMap[String, Object]()
    try {
      val tmpl = Source.fromFile("./template.tex").mkString
      //Process("mkdir " + outputDir!!)
      for ((sa, i) <- optimised.SAbasedMonitor.lookupTable.zipWithIndex) {
        val fileName = outputDir + "/sa" + i
        // dot -> ps
        Process(("echo " + sa._2.toDot) #| "dot -Tps" #> new File(fileName + ".ps") !!)
        // ps -> pdf
        Process("ps2pdf " + fileName + ".ps " + fileName + ".pdf" !!)
        // crop pdf
        Process("pdfcrop " + fileName + ".pdf " + fileName + ".pdf" !!)

        val saModel = new java.util.HashMap[String, Object]()
        saModel.put("pdf", "sa" + i + ".pdf")
        saModel.put("caption", sa._1.toTex)
        val stateModel = new java.util.HashMap[String, Object]()
        sa._2.states.foreach(state => stateModel.put(state.name, ""))
        saModel.put("states", stateModel)
        sasModel.put("sa" + i, saModel)
      }
      val model: java.util.HashMap[String, Object] = new java.util.HashMap[String, Object]()
      model.put("sas", sasModel)
      // write template as output
      Process(("echo " + engine.transform(tmpl, model)) #> new File(outputDir + "/report.tex") !!)
      println("INFO: Report written to directory " + outputDir + ".")
    } catch {
      case e: FileNotFoundException => println(e.getMessage); sys.exit()
      case e: RuntimeException => println(""); sys.exit()
    }
  }
}
