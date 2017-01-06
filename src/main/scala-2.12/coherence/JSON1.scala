package coherence

/**
  * Created by Levent Yilmaz on 9/24/2016.
  */

import coherence.JasonExpr.parseAll

import scala.util.parsing.combinator.{JavaTokenParsers, _}

class JSON1 extends JavaTokenParsers {
  var myObjs: List[MyContainer] = List()
  def obj: Parser[List[MyContainer]] =
    "{"~> repsep(member, ",") <~"}" ^^ (List() ++ _)

  def arr: Parser[List[Any]] =
    "["~> repsep(value, "]") <~"]"

  def member: Parser[MyContainer] =
    stringLiteral~":"~value ^^ {case name~":"~value => /*(name, value)*/ new MyContainer(name, value)}

  def value: Parser[Any] = (
    obj
      | arr
      | stringLiteral
      | floatingPointNumber ^^ (_.toDouble)
      | "null" ^^ (x => null)
      | "true" ^^ (x => true)
      | "false" ^^ (x => false)
    )
}

object JasonExpr extends JSON1 {
  def parseJSonExpression(inp: String): Unit = {
    println("input:" + inp)
    if ((parseAll(value, inp).successful) == true) {
      println("get: " + parseAll(value, inp).get)
      var testList: List[MyContainer] = parseAll(value, inp).get.asInstanceOf[List[MyContainer]]
      for (myElem <- testList)
        println("My object is " + myElem.myString + " " + myElem.myValue)
      println(parseAll(value, inp))
    }
    else println("Incorrect")
  }
}

class MyContainer(str: String, value: Any) {
  var myString : String = str
  var myValue : Any = value
}