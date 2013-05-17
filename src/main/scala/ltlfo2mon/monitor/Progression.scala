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
import ltlfo2mon.datatype._
import ltlfo2mon.datatype.Types._
import ltlfo2mon.Conf
import scala.collection._
import java.io._
import scala.util.Random
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
      case globally: Globally => And(prg(globally.chi, event, v), globally).eval
      //case eventually: Eventually => Or(prg(eventually.chi, event, v), eventually).eval
     
      case True() => True()
      case False() => False()
      case uop: Uop => if(uop.interpret(struct, event, v)) True() else False()
      case iop: Iop => if(iop.interpret(struct, v)) True() else False()
      case not: Not => Not(prg(not.phi, event, v)).eval
      case and: And => And(prg(and.phi, event, v), prg(and.psi, event, v)).eval
      case or: Or => Or(prg(or.phi,event, v), prg(or.psi, event, v)).eval
      case next: Next => next.phi
      case until: Until => Or(prg(until.psi, event, v), And(prg(until.phi, event, v), until).eval).eval
      case forall: Forall => {  
        var elems: immutable.Set[(Formula, Valuation)] = immutable.Set[(Formula, Valuation)]()    
        for(action <- event if action._1 == forall.p.name) {  
            var vNew: Valuation = v
            for((variable, i) <- forall.p.args.zipWithIndex) {
              vNew += ((variable.name, action._2(i)))
            }
            elems += ((forall.phi, vNew))         
        }        
        prg(ForallConj(elems), event, v)
      }
      case forallConj: ForallConj => {
        ForallConj(forallConj.elems.map{case (f,v) => (prg(f, event, v), v)}).eval        
      }        
    }
  }
 
  def process(event: Event, executedSubmons: mutable.HashMap[Int,Boolean3] = new mutable.HashMap[Int,Boolean3]()): Boolean3 = {
    rewrite = prg(rewrite, event, new Valuation)
        
    rewrite match { 
      case True()  => Top()
      case False() => Bottom()
      case _       => Unknown()
		 }
  }
  
  def process(trace: Trace, outputFile: String): (Boolean3,Int) = {    
    var result: Boolean3 = null
    var sizeOfEvents: List[Int] = List()
    var size: List[Int] = List()
    
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
        printWriter.println("#Index" + "\t" + "Event-size" + "\t" + "Formula-length")
      }
    }
    
    breakable { 
      for((event, i) <- trace.events.zipWithIndex) {
        val index = i+1
        Conf.index = index
        result = this.process(event)
        
        sizeOfEvents = event.size :: sizeOfEvents
        size = this.size :: size
  
        /*
         * experiments file output
         */
        printWriter match {
          case None => 
          case Some(printWriter) => {
            printWriter.println(index + "\t" + event.size + "\t" + this.size)
            }
        }
        
        if(Conf.verbose2) {
          println("\n### TIME " + index + " ###")          
          println("Rewritten formula: " + this.rewrite)
          println("Formula length: " + this.size) 
          println("Event size: " + event.size)
        }
        
        result match {
          case Top() | Bottom() => break
          case Unknown() =>
        }
      }
    }
    
    /*
     * print final results
     */
    if(Conf.verbose) {
      if(Conf.verbose2) println("\n\n### FINAL RESULT ###")
      else println("### FINAL RESULT ###")      
      println(this.name)    
      println("Formula: " + this.phi)
      println("Result: " + result + " after " + size.length + " events.")
      println("Trace length: " + size.length + '\n')
      
      println("Formula length: " + size.reverse.mkString(", "))
      println("Size of events: " + sizeOfEvents.reverse.mkString(", "))
      /*
       * measure runtime of monitor
       *
      val newMon = new Progression(this.phi, Config.struct)
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
  
  def size = rewrite.length
}
