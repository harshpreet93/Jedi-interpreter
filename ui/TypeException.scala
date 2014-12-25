package ui

//import JediException

class TypeException(msg: String) extends JediException(msg: String)
{
  override def toString() = msg

}