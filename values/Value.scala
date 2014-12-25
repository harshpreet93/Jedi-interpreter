package values

import expressions._
import ui._
trait Value extends Expression with Serializable{

	def execute(env: Environment): Value
}