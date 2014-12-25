package ui

class JediException(val msg: String = "Jedi Exception") extends Exception(msg) 
{
  override def toString() = msg
}