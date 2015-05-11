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
package ltlfo2mon

import datatype._

object Conf {
  var index: Int = 1

  /**
   * First-order structure
   */
  var struct = new Structure()

  /**
   * U-operators
   */  
  struct.addUoperator("v")
  struct.addUoperator("w") 
   
  /**
   * Constants and functions
   */
  // constant names surrounded by " to look identical to dynamically added constants by parser
  struct.addConst("\"0\"", 0)
  struct.addConst("\"1\"", 1)
  struct.addConst("\"2\"", 2)
  struct.addConst("\"3\"", 3)
  struct.addConst("\"4\"", 4)
  struct.addConst("\"5\"", 5)

  struct.addFunct("add", (args: Vector[Any]) => args(0).toString.toInt + args(1).toString.toInt)
  struct.addFunct("mul", (args: Vector[Any]) => args(0).toString.toInt * args(1).toString.toInt)
  struct.addFunct("sub", (args: Vector[Any]) => args(0).toString.toInt - args(1).toString.toInt)

  /**
   * I-operators
   */  
  struct.addIoperator("top", (args: Vector[Any]) => true)
  struct.addIoperator("bot", (args: Vector[Any]) => false)
  struct.addIoperator("leq", (args: Vector[Any]) => args(0).toString.toInt <= args(1).toString.toInt)
  struct.addIoperator("eq", (args: Vector[Any]) => args(0).toString.toInt == args(1).toString.toInt)
  struct.addIoperator("even", (args: Vector[Any]) => args(0).toString.toInt % 2 == 0)
  struct.addIoperator("odd", (args: Vector[Any]) => args(0).toString.toInt % 2 != 0)
  struct.addIoperator("div4", (args:Vector[Any]) => args(0).toString.toInt % 4 == 0)
  struct.addIoperator("regex", (args:Vector[Any]) => args(0).toString.matches(args(1).toString))
  
  // for RV'13 experiments
  struct.addIoperator("m", (args:Vector[Any]) => if((Conf.index+77) % (args(0).toString.toInt+80) == 0) true else false)
  struct.addIoperator("n", (args:Vector[Any]) => if((Conf.index+21) % (args(0).toString.toInt+80) == 0) true else false)
  struct.addIoperator("o", (args:Vector[Any]) => if((Conf.index+7) % (args(0).toString.toInt+80) == 0) true else false)
  struct.addIoperator("p", (args: Vector[Any]) => false, isRigid = true) // rigid false
  struct.addIoperator("q", (args: Vector[Any]) => args(0).toString.toInt % 2 == 0) // even  
  struct.addIoperator("r", (args:Vector[Any]) => if(Conf.index % (args(0).toString.toInt+1) == 0) true else false) // r(x): r becomes true max. 0-x worlds later  
  struct.addIoperator("s", (args:Vector[Any]) => if(Conf.index % 20 == 0) true else false)
  struct.addIoperator("t", (args:Vector[Any]) => if(Conf.index % (args(0).toString.toInt+args(1).toString.toInt+1) == 0) true else false)
  struct.addIoperator("u", (args:Vector[Any]) => true, isRigid = true) // rigid true

  /*
   * print options
   */  
  var verbose = true
  var verbose2 = false

  val path = "/tmp/"
 
  /*
   * trace parameter
   */
  var numOfTraces = 1
  var traceLength = 1000
  var eventSize = 5

  /*
   * formulae
   */
  var formulae: Array[String] = Array(
    // Absence: p is false, globally
    //"G A x:w.!p(x)",
    // Absence: p is false, after q
    //"G ((E x:w.q(x)) -> G A y:w.!p(y))"
    // Absence: p is false, between q and r
    //"G A x:w.(s(x) && !r(x) && F r(x)) -> (!p(x) U r(x))"
    //"G A x:w.s(x) -> F (q(x) && F r(x))"
    //"G A x:w.F s(x) && G A x:w.F r(x)"
    // Universality: p is true, between q and r
    //"G A x:w.(s(x) && !r(x) && F r(x)) -> (u(x) U r(x))",
    // Response: o responds to m, between n and r
    //"G A x:w.(n(x) && !r(x) && F r(x)) -> ((m(x) -> (!r(x) U (o(x) && !r(x)))) U r(x))",
    //"G A x:w.q(x) -> ((!u(x) && !r(x)) U (r(x) || ((u(x) && !r(x)) U (r(x) || ((!u(x) && !r(x)) U (r(x) || ((u(x) && !r(x)) U (r(x) || (!u(x) W r(x)) || G u(x)))))))))"
    //"G A x:w.q(x) -> ((!u(x) && !r(x)) U (r(x) || ((u(x) && !r(x)) U (r(x) || ((!u(x) && !r(x)) U (r(x) || ((u(x) && !r(x)) U (r(x) || (!u(x) W r(x)) || G u(x)))))))))",

    //"G A x:w.(q(x) && !p(x)) -> (!p(x) U (E y:w.t(x,y) && !p(x)))"
    /*
    //"G A x:w.!p1(x)" // 1
    G ((E x:w.q1(x)) -> G A y:w.!p1(y)), // 2
    G ((A x:w.q1(x)) -> G A y:w.!p1(y)), // 3
    G A x:w.((q1(x) && (!r1(x)) && F r1(x)) -> ((!p1(x)) U r1(x))), // 4
    G ((A x:w.q1(x) && (!r2(x)))-> (A y:w.!p1(y)) W E z:w.r2(z)), // 5
    G A x:w.((!q2(x)) || F (q2(x) && F p2(x))), // 6
    G A x:w.((q4(x) && (! r4(x)) && F r4(x)) -> (p4(x) U r4(x))), // 7
    G A x:w.(q9(x) U r9(x)), // 8
    G A x:w.((q8(x) && (! r8(x)) && F r8(x)) -> ((p8(x) -> ((! r8(x)) U (s8(x) && (! r8(x))))) U r8(x))), // 9
    G A x:w.((q8(x) && (! r8(x))) -> ((p8(x) -> ((! r8(x)) U (s8(x) && (!r8(x))))) W r8(x))), // 10
    G A x:w.(v(x) U A y:w.(v(y) U r6(x,y))), // 11
    G A x:w.A y:w.A z:w.(v(x,y,z) U r6(x,y,z)), // 12
    G A x:w.A y:w.A z:w.X v(x,y,z), // 13
    G (A x:w.((q1(x) && !r3(x)) -> ((!r3(x)) U (E y:w.p3(x,y) && !r3(x))))), // 14
    G A x:w.(e(x) -> (((!v(x)) && (!r(x))) U (r(x) || ((v(x) && (!r(x))) U
       (r(x) || (((!v(x)) && (!r(x))) U (r(x) || ((v(x) && (!r(x))) U
          (r(x) || ((!v(x)) W r(x)) || G v(x)))))))))), // 15
    G A x:w.A y:w.A z:w.((q4(x,y,z) && (! r4(x,y,z)) && F r4(x,y,z)) -> (p4(x,y,z) U r4(x,y,z))), // takes too long // 16
    G A x:w.((q1(x) && (! r3(x))) -> (! r3(x)) W (p2(x) && (! r3(x)))), // 17
    G A x:w.((q1(x) && (! r3(x))) -> (! r3(x)) U (p2(x) && (! r3(x)))) // 18
    */
      )
    
    /*
     * traces to monitor
     */
    var traces: Array[String] = Array(
      //"{w(12),w(10),w(9),w(9)},{w(10),w(5),w(6),w(10),w(11),w(4),w(8)},{w(8),w(10),w(14),w(13),w(6)}"
    )
}