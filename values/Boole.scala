package values
import expressions._
//case class Boole(init: Boolean) extends Value{

//this boolean class is a wrapper for scala's boolean class
  case class Boole(init: Boolean) extends Literal{
//class Boole(init: Boolean) extends Literal with Valuewith Serializable with Expression{
	var value: Boolean = false
	value = init
	def this(str: String)= this(str.toBoolean)

	def &&(other: Boole) = new Boole(value && other.value)
	def ||(other: Boole) = new Boole(value || other.value)
	def !(other: Boole) = new Boole(!other.value)
	def !=(other: Boole) = new Boole(value != other.value)
	
	override def toString() = value.toString()
	override def execute(env: Environment) = this 
}

object Boole {
  def test()
  {
    var env :Environment = new Environment
    val t = new Boole(true)
    val f = new Boole(false)
    val t1 = new Boole("true")
    var f1 = new Boole("false")
    f1 = f1.execute(env)
    println("...expected = " + (true && false) + ", actual = " + (t && f))
    println("...expected = " + (true && true) + ", actual = " + (t && t))
    println("...expected = " + (true || false) + ", actual = " + (t || f))
    println("...expected = " + (false || false) + ", actual = " + (f && f))
    println("...expected = " + (true == true) + ", actual = " + (t==t) )
    println("...expected = " + (true != true) + ", actual = " + (t!=t) )
    
    println("...expected = " + (true && false) + ", actual = " + (t1 && f1))
    println("...expected = " + (true && true) + ", actual = " + (t1 && t1))
    println("...expected = " + (true || false) + ", actual = " + (t1 || f1))
    println("...expected = " + (false || false) + ", actual = " + (f1 && f1))
    println("...expected = " + (true == true) + ", actual = " + (t1==t1) )
    println("...expected = " + (true != true) + ", actual = " + (t1!=t1) )
   }
  
  def main(args: Array[String]): Unit = {test()}
}