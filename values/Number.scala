package values
import expressions._
//import expressions.Literal
//case class Number(init: Double) extends Value{
case class Number(init: Double) extends Literal {
	var value: Double = init
	def this(init: String) = this(init.toDouble)
	def +(other: Number) = new Number(value+other.value)
	def -(other: Number) = new Number(value-other.value)
	def *(other: Number) = new Number(value*other.value)
	def /(other: Number) = new Number(value/other.value)
	def ==(other: Number) = new Boole(value == other.value)
	def <(other: Number) = new Boole(value < other.value)
	def >(other: Number) = new Boole(value > other.value)
	def <=(other: Number) = new Boole(value <= other.value)
	def >=(other: Number) = new Boole(value >= other.value)
	
	override def toString() = value.toString
	
	override def execute(env: Environment) = this
}

object Number {
  def test()
  {
    val n1 = new Number(10)
    val n2 = new Number(3.14)
    val n3 = new Number("10")
    val n4 = new Number("3.14")
    println("...expected = " + (10 + 3.14) + ", actual = " + (n1 + n2))
    println("...expected = " + (10 * 3.14) + ", actual = " + (n1 * n2))
    println("...expected = " + (10 - 3.14) + ", actual = " + (n1 - n2))
    println("...expected = " + (10 / 3.14) + ", actual = " + (n1 / n2))
    println("...expected = " + (10 < 3.14) + ", actual = " + (n1 < n2))
    println("...expected = " + (10 <= 3.14) + ", actual = " + (n1 <= n2))
    println("...expected = " + (10 >= 3.14) + ", actual = " + (n1 >= n2))
    println("...expected = " + (10 > 3.14) + ", actual = " + (n1 > n2))
    println("...expected = " + (10 == 3.14) + ", actual = " + (n1 == n2))
    
    println("...expected = " + (10 + 3.14) + ", actual = " + (n3 + n4))
    println("...expected = " + (10 * 3.14) + ", actual = " + (n3 * n4))
    println("...expected = " + (10 - 3.14) + ", actual = " + (n3 - n4))
    println("...expected = " + (10 / 3.14) + ", actual = " + (n3 / n4))
    println("...expected = " + (10 < 3.14) + ", actual = " + (n3 < n4))
    println("...expected = " + (10 <= 3.14) + ", actual = " + (n3 < n4))
    println("...expected = " + (10 >= 3.14) + ", actual = " + (n3 >= n4))
    println("...expected = " + (10 > 3.14) + ", actual = " + (n3 > n4))
    println("...expected = " + (10 == 3.14) + ", actual = " + (n1 == n2))
    
  }
  
  def main(args: Array[String]): Unit = {test()}
}