import scala.collection.mutable._
import scala.util.{Try, Success, Failure, Either}

object Forth {

  val dict = HashMap[String, (Int, Int) => Int]()
  val stack = Stack[Int]()

  def main(args: Array[String]): Unit = {
    dict += ("+" -> this.+, "-" -> this.-, "*" -> this.*, "/" -> this./)
      // "p" -> this.p)
    repl()
  }

  def repl() {
    var loop = true
    while (loop) {
      loop = Try(Console.readLine("forla> ")) match {
        case Success("q") => false
        case Success(line) => interpret(line)
        case Failure(_) => false
      }
    }
  }

  def interpret(line: String): Boolean = {
    for (each <- line.split(" ")) {
      dict.getOrElse(each, each) match {
        case "p" => println(stack.mkString(" "))
        case "bye" => return false
        case func: ((Int, Int) => Int) => execute(func)
        case _ => number(each)
      }
    }
    true
  }

  def execute(func: (Int, Int) => Int) {
    stack.push(func(stack.pop(), stack.pop()))
  }

  def number(token: String) {
    Try(token.toInt) match {
      case Success(num) => stack.push(num)
      case Failure(_) => println(s"${token}?")
    }
  }

  def +(x: Int, y: Int): Int = x + y
  def -(x: Int, y: Int): Int = x - y
  def *(x: Int, y: Int): Int = x * y
  def /(x: Int, y: Int): Int = x / y
  // def p() = println(stack)
}
