package expressions

//import values.Environment
import values._


case class Iteration(condition: Expression, body: Expression) extends SpecialForm{
  def execute(env: Environment) = 
  {
    var result: Value = Notification.UNKNOWN
    if(condition.execute(env).isInstanceOf[Boole])
    {
    	while(condition.execute(env).asInstanceOf[Boole].value ) result = body.execute(env)
    }
    result
  }
}