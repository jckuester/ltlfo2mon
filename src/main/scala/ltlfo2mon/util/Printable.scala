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
package ltlfo2mon.util

import ltlfo2mon.datatype._
import ltlfo2mon.datatype.Types._
import ltlfo2mon.monitor._
import scala.collection.mutable.HashMap
import ltlfo2mon.util.Printable._
import sys.process._
import scala.io.Source
import com.floreysoft.jmte.Engine
import java.io._

trait Printable {
  def toTex: String
  def toDot: String  
  def toLBTstr(encodedAtomsHat: HashMap[Formula, Int]): String
}

object Printable {
  implicit def any2Printable(any: Any): Printable = new Printable {

    def toDot = any match {
      case state: State => state.name.splitAt(1)._1 + "_" + state.name.splitAt(1)._2
      case qs: States =>  "digraph graphname {\n" +
          "\"\" [shape=none]\n" +
          // draw states
          qs.map{s => s.toDot + " [texlbl=\"$" + 
              s.formulae.map{f => f.toTex}.mkString(", ") +
      "$\"];"}.mkString("\n") + "\n\n" +
      // draw inital states
      qs.filter(_.isInitial).map{s => "\"\" -> " + s.toDot + ";"}.mkString("\n") + "\n" +
      // draw transitions
      qs.map{s => s.targetStates.map{out => s.toDot + " -> " + out.toDot + ";"}.mkString("\n")}.mkString("\n") +
      "\n}"
    }		

    def toTex = any match {  		  
      case state: State => state.name + ": " + state.formulae.map(f => f.toTex).mkString(",")
      case states: States => states.map(state => state.toTex).mkString("$", ", ", "$")      
    }


    /*
     * generates input string for LBT command line tool 
     */ 
    def toLBTstr(encodedAtomsHat: HashMap[Formula, Int]): String = any match {
      case _: True => "t"      
      case uop: Uop => "p" + encodedAtomsHat.get(uop).get.toString      
      case iop: Iop => "p" + encodedAtomsHat.get(iop).get.toString
      case not: Not => "! " + not.phi.toLBTstr(encodedAtomsHat) 
      case and: And => "& " + and.phi.toLBTstr(encodedAtomsHat) + " " + and.psi.toLBTstr(encodedAtomsHat) 
      case or: Or => "| " + or.phi.toLBTstr(encodedAtomsHat) + " " + or.psi.toLBTstr(encodedAtomsHat)
      case next: Next => "X " + next.phi.toLBTstr(encodedAtomsHat)
      case until: Until => "U " + until.phi.toLBTstr(encodedAtomsHat) + " " + until.psi.toLBTstr(encodedAtomsHat)
      case forall: Forall => "p" + encodedAtomsHat.get(forall).get.toString  
      case _ => ""
    }
  }  	

  def saCacheToTex(outputDir: String) = {
    // template engine
    var engine: Engine  = new Engine()
    var sasModel: java.util.HashMap[String, Object] = new java.util.HashMap[String, Object]()
    try{
      var tmpl = Source.fromFile("./template.tex").mkString   
      Process("mkdir " + outputDir!!)
          for((sa, i) <- SA.cache.zipWithIndex) {
            val fileName = outputDir + "/sa" + i
            // dot -> ps            
            Process(("echo " + sa._2.toDot) #| "dot -Tps"  #> new File(fileName + ".ps")!!)
            // ps -> pdf
            Process("ps2pdf " + fileName + ".ps " + fileName + ".pdf"!!)
            // crop pdf
            Process("pdfcrop " + fileName + ".pdf " + fileName + ".pdf"!!)   
  
            var saModel = new java.util.HashMap[String, Object]()
            saModel.put("pdf", "sa" + i + ".pdf")
            saModel.put("caption", sa._1.toTex)
            var stateModel = new java.util.HashMap[String, Object]()
            sa._2.foreach(state => stateModel.put(state.name, state.formulae.map(_.toTex).mkString(",")))
            saModel.put("states", stateModel)
            sasModel.put("sa"+i, saModel)
          }      
      var model: java.util.HashMap[String, Object] = new java.util.HashMap[String, Object]()
          model.put("sas", sasModel)
          // write template as output        
          Process(("echo " + engine.transform(tmpl, model)) #> new File(outputDir + "/report.tex")!!)
          println("INFO: Report written to directory " + outputDir + ".")
    } catch {
      case e: FileNotFoundException => println(e.getMessage()); exit      
      case e: RuntimeException => println(""); exit
    } 
  } 
}
