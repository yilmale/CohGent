package coherence

import edu.uci.ics.jung.algorithms.layout.AbstractLayout
import edu.uci.ics.jung.algorithms.layout.FRLayout2
import edu.uci.ics.jung.algorithms.layout.util.Relaxer
import edu.uci.ics.jung.graph._
import edu.uci.ics.jung.graph.util.Graphs
import edu.uci.ics.jung.visualization.VisualizationViewer
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer

import coherence.NetExpr.parseAll

import scala.util.parsing.combinator.{JavaTokenParsers, _}

/**
  * Created by Levent Yilmaz on 9/21/2016.
  */


class CoherenceModel(mName:String) {
  var g: Graph[String,Int] = null;
  var og: ObservableGraph[String,Int] =null;
  var ig = Graphs.synchronizedUndirectedGraph[String,Int](new  UndirectedSparseMultigraph[String,Int]())
  var activations:Map[String,Double]  = Map()
  var edgeWeights:Map[Int,Double] = Map()
  og = new ObservableGraph[String,Int](ig)
  g = og;

  def addNode(nodeName: String, defaultActivation: Double): Unit = {
    g.addVertex(nodeName)
    activations+=(nodeName -> defaultActivation)
  }

  def addEdge(source: String, target: String, weight: Double): Unit = {
    val edgeCount = g.getEdgeCount
    g.addEdge(edgeCount,source, target)
    edgeWeights+= (edgeCount -> weight)
  }

  def is (body : => Unit): CoherenceModel = {
    body
    this
  }

  def evidence(eName: String, defaultActivation:Double) = {
    addNode(eName,defaultActivation)
  }

  def facilitate(src: String, tgt: String, strength: Double)  = {
    addEdge(src,tgt,strength)
  }

  def graphIterator() {

    var V = g.getVertices
    var itr = V.iterator()
    System.out.println("Vertex List: ")
    while (itr.hasNext()) {
      var myN = itr.next()
      System.out.print("Node name: "+myN+ " ")
      System.out.println("Activation: "+ activations(myN))

      var incIter = g.getIncidentEdges(myN).iterator()
      while (incIter.hasNext()) {
        var myEdge = incIter.next()
        System.out.println("Incident edge number: "+ myEdge+" Weight is " + edgeWeights(myEdge) +" Source is "+g.getEndpoints(myEdge).getFirst+" Target is "+ g.getEndpoints(myEdge).getSecond)
        System.out.println("The opposite vertex of"+myN+" in edge"+ myEdge+" is  "+ g.getOpposite(myN,myEdge))
      }
    }
  }
}


object CoherenceModel  {
  var cm : CoherenceModel = _
  def apply(x:String): CoherenceModel = {
    cm = new CoherenceModel(x)
    cm
  }

  def evidence(eName: String, defaultActivation:Double) = {
    cm.addNode(eName,defaultActivation)
  }

  def facilitate(src: String, tgt: String, strength: Double)  = {
    cm.addEdge(src,tgt,strength)
  }

}

abstract class CognitiveModel
abstract class Net
abstract class Node extends Net
case class CogModel(netExpr: NetExpression, netspec: Network) extends CognitiveModel
case class NetExpression(name:String, portparams: Option[List[String]]) extends Net
case class Network(name: String, portlist: Option[List[Port]],portmapper: Option[Map[Port, Node]],evidences: List[Evidence],
                   hypotheses: List[Hypothesis], goals: List[Goal], actions: List[Action], coherenceConstraints: List[Constraint]) extends Net
case class Port(name:String, ptype:String) extends Net
case class Evidence(name:String,explanation:Option[String], defaultActivation:Double) extends Node
case class Hypothesis(name:String,explanation: Option[String],defaultActivation:Double) extends Node
case class Goal(name:String,explanation: Option[String],defaultActivation:Double) extends Node
case class Action(name:String,explanation: Option[String],defaultActivation:Double) extends Node
case class Constraint (source:List[String], target:String, ctype: String, strength:Double) extends Net
case class TargetMap(target:String, strength:Double) extends Net
case class Cogent(name: String, dataModel: List[String], behaviorModel: List[String], cModel:CogModel)

class CoherenceNet extends JavaTokenParsers {
  def cogentspec: Parser[Cogent] = "cogent"~>ident~dataspec~bspec~cmspec ^^
    {case name~data~behavior~cm => Cogent(name.toString(),data,behavior,cm)}
  def dataspec: Parser[List[String]] = "@data"~>repsep(datastmt,";") ^^ (List() ++ _)
  def datastmt: Parser[String] = stringLiteral ^^ {case stmt => stmt}
  def bspec: Parser[List[String]] = "@behavior"~>repsep(datastmt,";") ^^ (List() ++ _)
  def bstmt: Parser[String] = stringLiteral ^^ {case stmt => stmt}
  def cmspec: Parser[CogModel] = "@cognitiveModel"~>netexpr~netspec<~"endcogent" ^^
    {case netexpr~netspec => CogModel(netexpr,netspec)}
  def netexpr: Parser[NetExpression] = ident~"="~>ident~opt(parameterlist) ^^
    {case name~parameterlist => NetExpression(name.toString(),parameterlist)}
  def parameterlist: Parser[List[String]] = "("~>repsep(ident,",")<~")" ^^ (_.toString() :: List())
  def netspec: Parser[Network] = "net"~>ident~opt(portlist)~opt(portmapper)~evidencelist~hypothesislist~goallist~actionlist~constraintlist<~"endnet" ^^
    {case ident~portlist~portmapper~evidencelist~hypothesislist~goallist~actionlist~constraintlist =>
      Network(ident.toString(),portlist,portmapper,evidencelist,hypothesislist,goallist,actionlist,constraintlist)}
  def portlist : Parser[List[Port]] = "("~>inportlist~outportlist<~")" ^^
    {case inportlist~outportlist => inportlist ::: outportlist}
  def inportlist: Parser[List[Port]] = "inp"~":"~>repsep(inportmember, ",")<~";" ^^ (List() ++ _)
  def outportlist: Parser[List[Port]] = "outp"~":"~>repsep(outportmember, ",") ^^ (List() ++ _)
  def inportmember : Parser[Port] = ident ^^ {case name => Port(name.toString(),"in")}
  def outportmember : Parser[Port] = ident ^^ {case name => Port(name.toString(),"out")}
  def portmapper: Parser[Map[Port,Node]] = "["~>inportmapper~outportmapper<~"]" ^^
    {case inportmapper~outportmapper => inportmapper ++ outportmapper}
  def inportmapper: Parser[Map[Port,Node]] = repsep(inportmap, ",")<~";" ^^ (Map() ++ _)
  def outportmapper: Parser[Map[Port,Node]] = repsep(outportmap, ",") ^^ (Map() ++ _)
  def inportmap : Parser[(Port,Node)] = ident~"->"~ident ^^
    {case src~"->"~tgt => (Port(src.toString(),"in"),Evidence(tgt.toString(),Some(""),0.2))}
  def outportmap: Parser[(Port,Node)] =  ident~"->"~ident ^^
    {case src~"->"~tgt => (Port(tgt.toString(),"out"),Hypothesis(src.toString(),Some(""),0.2))}
  def evidencelist: Parser[List[Evidence]] = "@percepts"~>rep(evidencemember) ^^  (List() ++ _)
  def evidencemember: Parser[Evidence] = "evidence("~>ident~opt(explanation)~","~floatingPointNumber<~")" ^^
    {case ident~expl~","~value => Evidence(ident.toString(),expl,value.toDouble) }
  def explanation : Parser[String] = ":"~>stringLiteral ^^ {case expl => expl}
  def hypothesislist: Parser[List[Hypothesis]] = "@explanations"~>rep(hypothesismember) ^^  (List() ++ _)
  def hypothesismember: Parser[Hypothesis] = "hypothesis("~>ident~opt(explanation)~","~floatingPointNumber<~")" ^^
    {case ident~expl~","~value => Hypothesis(ident.toString(),expl,value.toDouble) }
  def goallist: Parser[List[Goal]] = "@goals"~>rep(goalmember) ^^  (List() ++ _)
  def goalmember: Parser[Goal] = "goal("~>ident~opt(explanation)~","~floatingPointNumber<~")" ^^
    {case ident~expl~","~value => Goal(ident.toString(),expl,value.toDouble) }
  def actionlist: Parser[List[Action]] = "@actions"~>rep(actionmember) ^^  (List() ++ _)
  def actionmember: Parser[Action] = "action("~>ident~opt(explanation)~","~floatingPointNumber<~")" ^^
    {case ident~expl~","~value => Action(ident.toString(),expl,value.toDouble) }
  def constraintlist: Parser[List[Constraint]] = "@constraints"~>rep(constraintmember) ^^  (List() ++ _)
  def constraintmember: Parser[Constraint] =  basicconstraint | compoundconstraint
  def basicconstraint: Parser[Constraint] = ident~"explains"~ident~"at"~floatingPointNumber ^^
    {case src~"explains"~tgt~"at"~value => Constraint(src.toString :: List(),tgt.toString,"explains",value.toDouble)} |
    ident~"contradicts"~ident~"at"~floatingPointNumber ^^
      {case src~"contradicts"~tgt~"at"~value => Constraint(src.toString :: List(),tgt.toString,"contradicts",value.toDouble)}
  def compoundconstraint : Parser[Constraint] = srclist~"explains"~ident~"at"~floatingPointNumber ^^
    {case srclist~"explains"~tgt~"at"~value => Constraint(srclist,tgt,"explains",value.toDouble) }
  def srclist: Parser[List[String]] = "["~>repsep(ident, ",")<~"]" ^^ (List() ++ _)

}


object NetExpr extends CoherenceNet {


  def parseExpression(inp: String): Unit = {
    println("input:" + inp)
    if ((parseAll(cmspec, inp).successful) == true) {
      println("get: " + parseAll(cmspec, inp).get)

      var myNet: Network = parseAll(cmspec, inp).get.asInstanceOf[CogModel].netspec
      println("The name of the network is " + myNet.name)

      myNet.portlist match {
        case Some(s) => {
          for (myElem <- s)
            println("My port is " + myElem.name + " with type " + myElem.ptype)
        }
        case None => println("No port listed")
      }

      myNet.portmapper match {
        case Some(s) => {
          for (myElem <- s) {
            if (myElem._2.isInstanceOf[Evidence] == true) {
              println("My port " + myElem._1.name + " is connected to " + myElem._2.asInstanceOf[Evidence].name)
            }
            else {
              println("My port " + myElem._1.name + " is connected to " + myElem._2.asInstanceOf[Hypothesis].name)
            }
          }
        }
        case None =>  println("No port mapping available")
      }

      for (myElem <- myNet.evidences)
        println("My data is " + myElem.name + " with activation " + myElem.defaultActivation)
      for (myElem <- myNet.hypotheses)
        println("My hypothesis is " + myElem.name + " with activation " + myElem.defaultActivation)
      for (myElem <- myNet.goals)
        println("My goal is " + myElem.name + " with activation " + myElem.defaultActivation)
      for (myElem <- myNet.actions)
        println("My action is " + myElem.name + " with activation " + myElem.defaultActivation)
      for (myElem <- myNet.coherenceConstraints)
        println("My constraint is " + myElem.source +" " + myElem.ctype + " "+ myElem.target + " at " + myElem.strength)

    }
    else println("invalid input")
  }
}