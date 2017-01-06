package coherence

/**
  * Created by Levent Yilmaz on 9/25/2016.
  */
import coherence.ParseExpr1.parseAll

import scala.util.parsing.combinator.{JavaTokenParsers, _}

abstract class Expr
case class Var(name:String) extends Expr
case class Number(num:Double) extends Expr
case class BinOp(operator:String, left: Expr, right: Expr) extends Expr

class ArithM extends JavaTokenParsers {
  def expr: Parser[Expr] = exprP | exprM | term
  def exprP: Parser[Expr] = term~"+"~expr  ^^ {case exp1~"+"~trm1 =>  BinOp("+",exp1,trm1)}
  def exprM: Parser[Expr] = term~"-"~expr ^^ {case exp2~"-"~trm2 =>  BinOp("-",exp2,trm2)}
  def term: Parser[Expr] = termM | termDiv | factor
  def termM: Parser[Expr] =  factor~"*"~term ^^ {case fact1~"*"~trm1 =>  BinOp("*",fact1,trm1)}
  def termDiv: Parser[Expr] = factor~"/"~term  ^^ {case fact2~"/"~trm2 => BinOp("/",fact2,trm2)}
  def factor: Parser[Expr] =  floatPN | exprPr
  def floatPN: Parser[Expr] = floatingPointNumber ^^ {case floatingPoint =>  Number(floatingPoint.toDouble)}
  def exprPr: Parser[Expr] = "("~>expr <~")" ^^ {case expr1 => expr1}
}

object ParseExpr1 extends ArithM {
  def parseExpression(inp: => String): Unit = {
    println("input:" + inp)
    println(parseAll(expr,inp))
    if (parseAll(expr,inp).get.isInstanceOf[Expr]) {
      println("this is expression")
    }
    var myExp: Expr = parseAll(expr,inp).get.asInstanceOf[Expr]
    myExp match {
      case BinOp("+",_,_) => println("Binary operation")
      case _ => println("something else")
    }
  }
}