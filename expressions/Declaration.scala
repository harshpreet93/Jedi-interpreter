package expressions

import values._

/**
 * Created by harshpreet93 on 11/18/14.
 */

//declaration of a variable
case class Declaration ( id: Identifier, ep: Expression) extends SpecialForm{
  def execute(env:Environment):Value = {
    env.put(id, ep.execute(env))
    Notification.OK
  }
}
