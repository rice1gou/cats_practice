import PrintableInstances._

final case class Cat(name: String, age: Int, color: String)

object Cat {
  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    def format(cat: Cat): String = {
      val name = cat.name
      val age = cat.age
      val color = cat.color
      s"$name is a $age year-old $color cat"
    }
  }
}
