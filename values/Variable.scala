package values
//import ui._
//import expressions._

case class Variable(cont: Value = null) extends Value {
  var content: Value = cont
  def execute(env: Environment) = 
  {
    content
  }
  override def toString() = 
  {
    "Variable"+"("+content.toString()+")"
  }
}