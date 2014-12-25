package expressions

import values._


case class Lambda(params: List[Identifier], exp: Expression) extends SpecialForm{
  
  def execute(env: Environment): Closure = 
  {
    Closure(params, exp, env)
  }

}