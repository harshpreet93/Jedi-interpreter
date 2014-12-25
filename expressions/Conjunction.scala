package expressions

import ui._
import values._
/**
 * Created by harshpreet93 on 11/18/14.
 */


case class Conjunction(operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value =
  {
    var i: Int = 0
    var result: Boolean = true
    while(result && i < operands.length)
    {
      if(operands.length <= i) throw new JediException("Index out of range")
      val v = operands(i).execute(env)
      if(! v.isInstanceOf[Boole]) throw new TypeException("Inputs to && must be Booles")
      val b = v.asInstanceOf[Boole]
      i+=1
      result = b.value
    }
    Boole(result)
  }
}
