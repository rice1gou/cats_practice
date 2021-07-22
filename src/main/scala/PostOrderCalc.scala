import cats.data.State
import cats.syntax.applicative._

object PostOrderCalc {
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" =>operator(_+_)
    case "-" =>operator(_-_)
    case "*" =>operator(_*_)
    case "/" =>operator(_/_)
    case num =>operand(num.toInt)
  }

  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(0.pure[CalcState])((a, b) => a.flatMap(_=>evalOne(b)))
  }

  def evalInput(input: String): Int = {
    evalAll(input.split(" ").toList).runA(Nil).value
  }

  def operand(num: Int): CalcState[Int] = {
    State[List[Int], Int]{ stack =>
      (num::stack, num)
    }
  }

  def operator(func: (Int, Int) => Int): CalcState[Int] = {
    State[List[Int], Int]{
      case a::b::tail => (tail, func(a, b))
      case _ => sys.error("Fail!")
    }
  }
}
