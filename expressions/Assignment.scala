package expressions

import ui._
import values.{Environment, Notification, Value, Variable}

//vbl represents the name of the variable and update is its new value
case class Assignment(vbl: Identifier, update: Expression) extends SpecialForm{
  
	def execute(env: Environment): Value = 
	{
	  if(vbl.execute(env).isInstanceOf[Variable]) 
	  {
	    var vbl1 = vbl.execute(env).asInstanceOf[Variable]
	    vbl1.content = update.execute(env)
	    //vbl1
	    Notification.DONE 
	  }
	  else throw new TypeException("Cannot assign to a non-variable")
	
	}
	
	override def toString() = 
	{
	  ""
	}
}