package expressions

import ui._
import values._

/**
 * Created by harshpreet93 on 11/18/14.
 */

//if(a) b else c
//if(a) b

//if (false) 3
//Notification.UNKNOWN


//use short circuit evaluation
//needs laziness to work correctly
case class Conditional(condition: Expression, consequent: Expression, alternate: Expression = null) extends SpecialForm {

    def execute(env: Environment): Value =
    {
    	if(!condition.execute(env).isInstanceOf[Boole]) throw new TypeException("Condition must be a boole")
        if(condition.execute(env) == new Boole(true))
        {
          consequent.execute(env)
        }
        else if(condition.execute(env) == new Boole(false) && alternate != null )
        {
          alternate.execute(env)
        }
        else
        {
          Notification.UNKNOWN
        }
    }
}
