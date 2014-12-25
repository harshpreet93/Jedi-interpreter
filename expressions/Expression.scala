package expressions

import values._

//An expression is an overarching concept. any class that has this trait is an expression
trait Expression {
	def execute(env: Environment): Value
}