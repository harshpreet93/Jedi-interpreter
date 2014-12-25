package values


import java.io.Serializable
import scala.util.parsing.combinator._
import expressions._
import values._
import ui._
import expressions.Block
import expressions.Block

class SithParsers extends RegexParsers{

  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression") ^^
  {
    case c => c
  }

    def declaration: Parser[Expression] = "def"~identifier~"="~expression ^^
    {
      case "def"~identifier~"="~expression => new Declaration(identifier, expression)
    }

    def conditional: Parser[Conditional] = "if"~"("~expression~")"~expression~opt("else"~expression) ^^
    {
      case "if"~"("~ex~")"~exp~Some("else"~exp2) => new Conditional( ex, exp, exp2 )
      case "if"~"("~ex~")"~exp~None => new Conditional(ex, exp)
    }

    def disjunction: Parser[Expression] = conjunction~rep("||"~>conjunction) ^^
    {
      case c~List() => c
      case c~rest => new Disjunction(c::rest)
    }

    def conjunction: Parser[Expression] = equality~rep("&&"~>equality) ^^
    {
      case e~List() => e
      case e~rest => new Conjunction(e::rest)
    }

  def equality : Parser[Expression] = inequality~rep("=="~>inequality) ^^
  {
    case c~Nil => c
    case c~rest => new FunCall(new Identifier("equals"), c::rest)
  }

  def inequality: Parser[Expression] = sum~rep("<" ~> sum) ^^
  {
    case s~List() => s
    case s~rest => new FunCall(new Identifier("less"), s::rest)
  }

  def sum: Parser[Expression] =
    product ~ rep(("+"|"-") ~ product ^^ {case "+"~s=>s case "-"~s=>negate(s)})^^
      {
      case p~List()=> p
      case p~rest=>FunCall(Identifier("add"), p::rest)
    }

  def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Number(0)
    FunCall(sub, List(zero, exp))
  }


//  Our solution will be to convert differences into sums of negations and quotients into products of inverses:
  def product: Parser[Expression] = funcall~rep(("*"|"/")~funcall ^^
  {
    case "*"~s => s
    case "/"~s => invert(s)
  }) ^^
  {
    case p~List() => p
    case p~rest => FunCall(Identifier("mul"), p::rest)
  }

  def invert(f: Expression): Expression =
  {
      val div = Identifier("div")
      val one = Number(1)
      FunCall(div, List(one, f))
  }

  def funcall: Parser[Expression] = term~opt(operands)^^
  {
    case term ~ None => term
    case term ~ Some(operands) => FunCall( term.asInstanceOf[Expression], operands )
  }

  def operands: Parser[List[Expression]] = "("~>opt(expression~rep(","~>expression))<~")" ^^
  {
    case None => List()
    case Some( t ~ List() ) => List(t)
    case Some(exp~rest) => exp::rest
    
  }

  def identifier: Parser[Identifier] = """[a-zA-Z][0-9a-zA-Z]*""".r ^^
    {
      case c => new Identifier(c)
    }

  def term: Parser[Expression] = ( assignment | iteration | deref | lambda | block | literal | identifier  | "("~>expression<~")" ) 

  def literal: Parser[Literal] = boole | number

  def boole: Parser[Boole] = ("true" | "false") ^^
  {
    case c => new Boole(c)
  }

  def number: Parser[Number] = """(\+|-)?[0-9]+(\.[0-9]+)?""".r ^^
    {
      case c => new Number(c)
    }
  
  def parameters: Parser[List[Identifier]] =  "(" ~> opt(identifier ~ rep(","~>identifier))<~")" ^^
  {
	  case None => Nil 
	  case Some(e ~ Nil) => List(e) 
	  case Some(e ~ exps) => e::exps 
	  case _ => Nil
  }
  
  //BLOCK ::- {EXPRESSION (; EXPRESSION)*}
  
  def block: Parser[Expression] = "{"~>expression ~ rep(";"~>expression)<~"}" ^^
  {
    case expression ~ Nil => Block(List(expression))
    case expression ~ exps => Block(expression::exps)
  }
  def lambda: Parser[Expression] = "lambda"~>parameters~expression ^^
  {
    case p~e => new Lambda(p, e)
  }
  
//  def obj: Parser[Expression] = "object" ~ block ^^
//  {
//    
//  }
//  
//  def access: Parser[Expression] = term~opt("."~>identifier) ^^
//  {
//    
//  }
  def assignment: Parser[Expression] = identifier~"="~expression ^^
  {
    case id~"="~ex => new Assignment(id, ex)
  }
  
  def iteration: Parser[Expression] = "while"~"("~expression~")"~expression ^^
  {
    case "while"~"("~e1~")"~ e2 => new Iteration(e1, e2)
  }
  
  def deref: Parser[Expression] = "["~expression~"]" ^^
  {
    case "["~expression~"]" => new FunCall(Identifier("contentdd"), List(expression))
  }
}