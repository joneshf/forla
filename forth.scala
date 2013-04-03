import scala.collection.mutable._
import scala.util.{Try, Success, Failure, Either}

object Forth {

  val dict = Map[String, () => Any]()
  var stack = Stack[Int]()

  def main(args: Array[String]) {
    dict += ("+" -> this.+_, "-" -> this.-_, "*" -> this.*_, "/" -> this./_,
      "." -> this.print)
    repl()
  }

  def repl() {
    var loop = true
    while (loop) {
      loop = Try(Console.readLine("forla> ")) match {
        case Success("bye") => false
        case Success(line) => interpret(line)
        case Failure(_) => false
      }
    }
  }

  def interpret(line: String): Boolean = {
    for (each <- line.split(" ")) {
      dict.get(each) match {
        case Some(func) => execute(func)
        case None => number(each)
      }
    }
    true
  }

  def execute(func: () => Any) {
    Try(func()) match {
      case Success(_) => return
      case Failure(e: NoSuchElementException) => println("Stack underflow")
      case Failure(e) => println(s"Some other error: ${e.getMessage}")
    }
  }

  def number(token: String) {
    Try(token.toInt) match {
      case Success(num) => stack.push(num)
      case Failure(_) => println(s"${token} ?")
    }
  }

  def +() = stack.push(stack.pop() + stack.pop())
  def -() = stack.push(stack.pop() - stack.pop())
  def *() = stack.push(stack.pop() * stack.pop())
  def /() = stack.push(stack.pop() / stack.pop())
  def print() = println(stack.mkString(" "))
}
