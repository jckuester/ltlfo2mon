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

object Ltlfo2sa {
  
  val cmdParser = new scopt.immutable.OptionParser[Config]("ltlfo2sa", "v1.0 beta") { 
    def options = Seq(
        arg("<ltlfo-formula>", "LTLFO formula.") { (v: String, c: Config) => c.copy(formula = v) },
        arg("<filename>", "Look-up table, which contains precomputed SAs.") { (v: String, c: Config) => c.copy(saCacheFile = v) },
        opt("r", "report", ",<directory>", "Store latex-report illustrating precomputed SAs in <directory>.") { (v: String, c: Config) => c.copy(reportDir = v) }
        )}
  /*
   * 
  def main(args: Array[String]): Unit = {
    cmdParser.parse(args, Config()) map { config =>      
      val formulaParser = new FormulaParser(Conf.struct)
      
      formulaParser.parse(config.formula) match {
       case None => return
       case Some(formula) => {
         SA.setSAcache(formula)         
         SA.storeSAcacheToFile(config.saCacheFile)         
       }
      }
      if(config.reportDir != "") {
        Printable.saCacheToTex(config.reportDir)
      }      
    } getOrElse {
      // arguments are bad, usage message will have been displayed
    }
  }
  *
  */

  /*
   * config for scopt
   */
  case class Config(formula: String = "", saCacheFile: String = "", reportDir: String = "")
}

