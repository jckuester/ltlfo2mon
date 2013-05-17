package ltlfo2mon.util.parsing

import scala.util.parsing.combinator._
 
/**
 * Parser combinator for Operator-precedence parser(http://en.wikipedia.org/wiki/Operator-precedence_parser).
 */
trait OperatorPrecedenceParsers extends Parsers {
 
  trait Op[+T,U] {
    def lhs:Double = 0
    def rhs:Double = 0
    def parse:Parser[T]
  }
 
  case class Infix[T,U]
  ( override val lhs:Double
  , override val rhs:Double)
  ( override val parse:Parser[T])(val map:(T,U,U) => U) extends Op[T,U]
 
  case class Prefix[T,U](override val rhs:Double)(override val parse:Parser[T])(val map:(T,U) => U) extends Op[T,U]
  case class Suffix[T,U](override val lhs:Double)(override val parse:Parser[T])(val map:(T,U) => U) extends Op[T,U]
 
  def operators[T,U](opseq:Op[T,U]*)(innermost:Parser[U]) = new Parser[U] {
 
    type Ops = List[(Op[T,U],T)]
    type Out = List[U]
 
    val (prefixOps, suffixOps) = opseq.partition( _.isInstanceOf[Prefix[_,_]] )
 
    def findPrefix(ops:Ops, out:Out, in:Input):ParseResult[U] = {
      prefixOps.iterator.map(e => e -> e.parse(in))
        .collectFirst {
          case (op, Success(o, in2)) => findPrefix(op -> o :: ops, out, in2)
        }
        .getOrElse(innermost(in)
          .flatMapWithNext(o => in => findSuffix(ops, o :: out, in)))
    }
    
    def fold(lhs:Double, ops:Ops, out:Out):(Ops,Out) =
      ops match {
        case (op, o)::ops if op.rhs < lhs =>
          fold(lhs, ops, (( (op, out): @unchecked ) match {
            case (op:Prefix[T,U], x::xs) => op.map(o, x) :: xs
            case (op:Suffix[T,U], x::xs) => op.map(o, x) :: xs
            case (op: Infix[T,U], y::x::xs) => op.map(o, x, y) :: xs
          }))
        case _ => ops -> out
      }
 
    def findSuffix(ops:Ops, out:Out, in:Input):ParseResult[U] =
      suffixOps.iterator.map(e => e -> e.parse(in))
        .collectFirst {
          case (op, Success(o, in)) =>
            val $ = fold(op.lhs, ops, out)
            (if (op.isInstanceOf[Infix[_,_]])
              findPrefix _ else
              findSuffix _ ) ((op, o) :: $._1, $._2, in)
        }
        .getOrElse(Success(fold(1/0.0, ops, out)._2.head, in))
 
    override def apply(in:Input):ParseResult[U] = findPrefix(Nil, Nil, in)
 
  }
}
