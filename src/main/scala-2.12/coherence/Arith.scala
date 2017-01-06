package coherence

/**
  * Created by Levent Yilmaz on 9/24/2016.
  */

import coherence.ParseExpr.parseAll

import scala.util.parsing.combinator.{JavaTokenParsers, _}

class Arith extends JavaTokenParsers {
  def expr: Parser[Any] = term~rep("+"~term | "-"~term)
  def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
}

object ParseExpr extends Arith {
  def parseExpression(inp: String): Unit = {
    println("input:" + inp)
    println(parseAll(expr,inp))
  }
}