package coherence

/**
  * Created by Levent Yilmaz on 11/28/2016.
  */

abstract class TemplateModel(var name:String) {
  def exec(fname:String)
}


object CogentEvaluator {
  def evaluate(inp: => String): Unit = {
    println(inp)
    ParseExpr1.parseExpression(inp)
  }

  def evaluateJson(inp: => String): Unit = {
    println(inp)
    JasonExpr.parseJSonExpression(inp)
  }

  def evaluateNet(inp: => String) : Unit = {
    println(inp)
    NetExpr.parseExpression(inp)

  }
  def test() {
    val inp: String = "(1+2)*3"
    //ParseExpr.parseExpression(inp)

    val inpjson: String =
      """
    { "address"  : 15,
    "name" : "Levent" }
    """

    // JasonExpr.parseJSonExpression(inpjson)

    evaluate ("1+2*3")

    val inpNet : String =
      """
     cogent producer

       @data
         var x : String = name
         var i:Int =0

       @behavior
         def A1() : Int  =>
           var A1x : String = A1name
           var A1i:Int =0


         def A2() : String =>
           var A2x : String = A2name
           var A2i:Int =0


       @cognitiveModel
        cm = X(a,b)

        net X(inp: P,Q; outp: Z)
          [P->A, Q->B; E->Z]
            @percepts
              evidence(A,0.5)
              evidence(B:"Test2",0.7)


            @explanations
              hypothesis(C:"Test3",0.3)
              hypothesis(D: "Test4",0.9)
              hypothesis(E,0.6)


            @goals
              goal(G:"MyGoal", 0.7)


            @actions
              action(A1: "MyAction", 0)


            @constraints
              C explains B at 0.5
              C contradicts D at 0.7
              [D, E] explains A at 0.6

        endnet
       endcogent
    """

    val data :String = inpNet.split("@data").lift(1).get.
      split("@behavior").lift(0).get

    println(data)

    val behavior :String  = inpNet.split("@behavior").lift(1).get.
      split("@cognitiveModel").lift(0).get

    println(behavior)

    val actions = behavior.split("def")

    println("Actions are....")
    for (i <- actions) {
      println(i)
    }



    val cm :String  = "@cognitiveModel"+
      inpNet.split("@cognitiveModel").lift(1).get

    println(cm)




    NetExpr.parseExpression(cm)
  }

  def testTemplate(): Unit = {

    val c = new TemplateModel("Test") {
      var x : String = name
      var i:Int =0
      var m: Map[String,() => Unit] = Map(
        ("x", () => {
          i=i+1
          println(i+ " hello from func1")
        }),
        ("y", () => {i=i+10;println(i+ " hello from func2")})
      )


      def testfunc2() {println("hello"); println(x)}

      override def exec(funcName: String): Unit = {
        var myfn = m(funcName)
        myfn()
      }

    }

    println(c.x)
    c.testfunc2()
    c.exec("x")
    c.exec("y")
  }

}
