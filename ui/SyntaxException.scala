package ui
import scala.util.parsing.combinator._
//import JediException

class SyntaxException(val result: Parsers#Failure = null) extends JediException("Syntax error")
{
  
}