package ui

//import JediException

class UndefinedException(msg: String) extends JediException(msg: String)
{
  override def toString() = msg
}