package values
import ui._
import expressions._


case class Closure(params: List[Identifier], body: Expression, defEnv: Environment) extends Value {
   def apply(args: List[Value]): Value = 
   {
		if(params.length != args.length) throw new TypeException("Params and args cannot have different length")
		var tempEnv : Environment = new Environment(defEnv)
		
		tempEnv.put(params, args)
		
		body.execute(tempEnv)
   }
   
   def execute(env: Environment): Value = 
   {
		   Notification.UNKNOWN 
   }
   
   override def toString(): String = 
   {
     ""
   }
}