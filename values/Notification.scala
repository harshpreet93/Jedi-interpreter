package values

//import values.Value

class Notification(msg: String) extends Value {
	def execute(env: Environment): Value = Notification.UNKNOWN
	override def toString() = msg
}

//"variable updated", "binding created", "unknown", "error", and "OK".
object Notification {
  val UNKNOWN = new Notification("unknown")
  val OK = new Notification("ok")
  val DONE = new Notification("done")
  val ERROR = new Notification("error")
  val BINDING_CREATED = new Notification("binding created")
  val VARIABLE_UPDATED = new Notification("variable updated")
}