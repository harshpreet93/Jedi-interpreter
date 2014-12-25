package expressions

import ui._
import values._

//args is Nil by default because there are functions that need no arguments
//opcode is the name of the function
case class FunCall(opcode: Expression, args: List[Expression] = Nil) extends Expression{
	def execute(env: Environment) = 
	  {
		var tempArgs = args.map(_.execute(env))
		try
		{
			var opExe = opcode.execute(env)
			if(!opExe.isInstanceOf[Closure]) throw new TypeException("cannot be cast to Closure")
			opExe.asInstanceOf[Closure](tempArgs)
		}
		catch
		{
		  case t: UndefinedException => system.execute(opcode.asInstanceOf[Identifier], tempArgs)
		}
	  }
}