sealed trait Json

case class JsonObject(get: Map[String, Json]) extends Json

case class JsonString(get: String) extends Json

case class JsonNumber(get: Int) extends Json

case object JsonNull extends Json

trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

object JsonWriterInstances {
  implicit val StringWriter: JsonWriter[String] = new JsonWriter[String] {
    def write(value: String): Json = {
      JsonString(value)
    }
  }

  implicit val personWriter: JsonWriter[Person] = new JsonWriter[Person] {
    def write(value: Person): Json = {
      JsonObject(Map(
        "name" -> JsonString(value.name),
        "email" -> JsonString(value.email)
      ))
    }
  }
}
