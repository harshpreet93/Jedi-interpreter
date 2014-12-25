package expressions

import values._

//A literal is a raw value such as 2, true, and 2.3
class Literal extends Expression with Value{//with Serializable{
	//var value: Expression
  def execute(env: Environment): Value = this
}