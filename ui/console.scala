package ui
import values._
import expressions._
object console {
   val parsers = new SithParsers // for now
   val globalEnv = new Environment()

   def execute(cmmd: String): String = {
      val tree = parsers.parseAll(parsers.expression, cmmd)
      tree match {
         case t: parsers.Failure => throw new SyntaxException(t)
         case _ => "" + tree.get.execute(globalEnv)
      }
   }
   
    def repl {
      var more = true
      // declare locals
      while(more) {
        println("-> ")
        val cmmd = readLine()
        if(cmmd == "quit"){
          println("Bye!")
          more = false
        }
        else {
         try {
            // read/execute/print
        	 println( console.execute(cmmd) )
         } 
         catch {
            case e: SyntaxException => {
            	 //println("Exception")
               println(e.msg)
               println(e.result.msg)
               println("line # = " + e.result.next.pos.line)
               println("column # = " + e.result.next.pos.column)
               println("token = " + e.result.next.first)
            }
            case u: UndefinedException =>
              {
                println(u.msg)
              }
            case ty: TypeException =>
              {
                println(ty.msg)
              }
            case j: JediException =>
              {
                println(j.msg)
              }
            case t: Throwable =>
              {
                println("Something went wrong")
                t.printStackTrace()
                more = false
              }
            // handle other types of exceptions
         } finally {
            Console.flush 
         }
        }
      }
   }
    
   def main(args: Array[String]): Unit = { repl }
}