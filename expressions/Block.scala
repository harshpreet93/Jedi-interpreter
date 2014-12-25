package expressions

import values._

//A block of jedi code which is execute with reference to its own separate environment
case class Block(locals: List[Expression]) extends SpecialForm {
   def execute(env: Environment) = 
   {
	   var tempEnv: Environment = new Environment(env)
	   locals.map( _.execute(tempEnv) ).last
   }
}