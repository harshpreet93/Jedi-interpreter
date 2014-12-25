package ui
import values._
import expressions._

object system {
  
  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add" => add(args)
      case "sub" => sub(args)
      case "mul" => mul(args)
      case "div" => div(args)
      case "equals" => equals(args)
      case "less" => less(args)
      case "not" => not(args)
      case "contentdd" => contentdd(args)
      case "var" => makeVar(args)
      case _ => throw new UndefinedException("Undefined function: "+opcode.name)
    }
  }
  
  private def contentdd(args: List[Value]) = 
  {
    if(args.isEmpty) throw new TypeException("contentdd expects 1 input")
    if(args.length >1) throw new TypeException("contentdd cannot have multiple inputs")
    if(args.head.isInstanceOf[Variable])
    {
      args.head.asInstanceOf[Variable].content
    }
    else
    {
      throw new TypeException("contentdd can only be applied to variables")
    }
  }
  
  private def makeVar(args: List[Value]) = 
  {
    if(args.isEmpty) throw new TypeException("makeVar expects 1 input")
    if(args.length >1) throw new TypeException("makeVar cannot have multiple inputs")
    new Variable(args.head)
  }
  
  private def add(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("addition expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all addition inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_+_)
  }
  
  private def sub(vals: List[Value]): Value = {
	if (vals.isEmpty) throw new TypeException("subtraction expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all subtraction inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_-_)  
  }
  
  private def mul(vals: List[Value]): Value = {
	if (vals.isEmpty) throw new TypeException("multiplication expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all multiplication inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_*_)  
  }

  private def div(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("division expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all division inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_/_) 
  }
  
  private def equals(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("equals expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all equals inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    if(ok.length < 2) throw new TypeException("equals expects >= 2 inputs")
    new Boole( args2.tail.map(_ == args2.head).filter(_ != new Boole(true)).length == 0 )
  }
  
  private def less(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("less expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all less inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    
    if(ok.length < 2) throw new TypeException("less expects >= 2 inputs")
    new Boole( args2.zip(args2.tail)
    				.map(x => x._1 < x._2)
    				.filter(_ != new Boole(true) )
    				.length == 0 )
  }
  
  private def not(vals: List[Value]): Value = 
  {
    if (vals.isEmpty) throw new TypeException("not expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Boole])
    if (ok.length < vals.length) throw new TypeException("all not inputs must be Booles")
    val args2 = vals.map(_.asInstanceOf[Boole])
    if(ok.length != 1) throw new TypeException("not expects only 1 input")
    new Boole(false).!(args2(0))
  }
  // etc.
}