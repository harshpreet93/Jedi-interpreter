package expressions

import ui._
import values._

//An identifier is a name. It can be the name of a variable or a funcall.
case class Identifier(name: String) extends Expression with Serializable{
	//var id: String = name
	def execute(env: Environment): Value = 
	{
	  if(env.find(this) == Notification.UNKNOWN ) throw new UndefinedException("Undefined identifier: "+name)
	  else env.find(this)
	}
}