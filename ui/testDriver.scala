package ui
import values._
import expressions._
object testDriver {
  
  def main(args: Array[String]): Unit = 
  { 
    println("testing Boole")
    Boole.test()
    
    println("testing Number")
    Number.test()
    
    println("testing Environment")
    Environment.test()
  }

}