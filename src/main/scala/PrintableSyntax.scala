object PrintableSyntax {
  implicit class PrintableSyntaxOps[A](value: A) {
    def format(implicit p: Printable[A]): String = {
      p.format(value)
    }
    def print(implicit p: Printable[A]): Unit = {
      println(format(p))
    }
  }
}
