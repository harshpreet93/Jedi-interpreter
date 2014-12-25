package values

import expressions._

import scala.collection.mutable.HashMap

//Environment is a tool for simulating scope in the jedi language
class Environment(nextEnv: Environment = null) extends HashMap[Identifier, Value] with Value {
	var nextEnvironment: Environment = nextEnv
	//var thisEnvironment: HashMap[Identifier, Value] = new HashMap[Identifier, Value]
	
	def put(names: List[Identifier], vals: List[Value])
	{
	  var pairs: List[(Identifier, Value)] = names.zip(vals)
	  pairs.map(x => super.put(x._1, x._2))
	}
	
	def find(id: Identifier): Value = 
	{
	  var res: Option[Value] = super.get(id)
	  res match
	  {
	    case Some(v) => { v }
	    case None =>
	    {
	    	if(nextEnv == null) Notification.UNKNOWN 
	    	else nextEnv.find(id)
	    }
	  }
	}
	def execute(env: Environment): Value = Notification.UNKNOWN 
}

object Environment
{
	def test()
	{
	val num1 = new Number(100.0)
    val num2 = new Number(42.0)
    val t = new Boole(true)
    val f = new Boole(false)
    
    val n1 = new Identifier("n1")
    val n2 = new Identifier("n2")
    val n3 = new Identifier("n3")
    val n4 = new Identifier("n4")
    val b1 = new Identifier("b1")
    val b2 = new Identifier("b2")
    val b3 = new Identifier("b3")
    val b4 = new Identifier("b4")
    val x = new Identifier("x")
    val globalEnv = new Environment()
    val env1 = new Environment(globalEnv)
    val env2 = new Environment(env1)
    globalEnv.put(List(n1, n2, b1, b2), List(num1, num2, t, f))
    env1.put(List(n3, b3), List(num1 + num2, num1 < num2)) 
    env2.put(List(n2, n4, b3, b4), List(num2 - num1, num1 / num2, num2 < num1, f))
    
    println("...expected = " + num1 + " actual = " + env2.find(n1))
    println("...expected = " + (num2 - num1) + " actual = " + env2.find(n2))
    println("...expected = " + (num2 + num1) + " actual = " + env2.find(n3))
    println("...expected = " + (num1 / num2) + " actual = " + env2.find(n4))
    println("...expected = " + (t) + " actual = " + env2.find(b1))
    println("...expected = " + (f) + " actual = " + env2.find(b2))
    println("...expected = " + (num2 < num1) + " actual = " + env2.find(b3))
    println("...expected = " + (f) + " actual = " + env2.find(b4))
    println("...expected = " + Notification.UNKNOWN + " actual = " + env2.find(x))
    
    env2.put(new Identifier("a"), new Number(35))
    env2.put(new Identifier("ab"), new Number(30))
    println("...expected = 35"+" actual = " + env2.find(new Identifier("a")))
    println("...expected = 30"+" actual = " + env2.find(new Identifier("ab")))
    
    env2.put(new Identifier("notification"), Notification.UNKNOWN )
    env2.put(new Identifier("bool"), new Boole("true"))
    println("...expected = "+(Notification.UNKNOWN)+" actual = " + env2.find(new Identifier("notification")))
    println("...expected = true"+" actual = " + env2.find(new Identifier("bool")))
    
	}
}