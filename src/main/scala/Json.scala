import JsonWriterInstances._

object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = {
    w.write(value)
  }
}
//Json.toJson(Person("keisuke", "sample@gmail"))
//The compiler spots that we’ve called the toJson method without providing the implicit parameters.
// It tries to fix this by searching for type class instances of the relevant types and
// inserting them at the call site:
//Json.toJson(Person("keisuke", "sample@gmail"))(personWriter)

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = {
      w.write(value)
    }
  }
}
//Person("Uno", "sample@email").toJson
//the compiler searches for candidates for the implicit parameters and fills them
//Person("Uno", "sample@email").toJson(personWriter)