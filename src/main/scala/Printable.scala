trait Printable[A] {self =>
  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] = {
    new Printable[B] {
      def format(value: B): String = {
        self.format(func(value))
      }
    }
  }
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    def format(input: String): String = input
  }
  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    def format(input: Int): String = input.toString
  }
  implicit val boolPrintable: Printable[Boolean] = new Printable[Boolean] {
    def format(input: Boolean): String = if (input) "yes" else "no"
  }
}

object Printable {
  def format[A](input: A)(implicit p: Printable[A]): String = {
    p.format(input)
  }

  def print[A](input: A)(implicit p: Printable[A]): Unit = {
    println(format(input))
  }
}
