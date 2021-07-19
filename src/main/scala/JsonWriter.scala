sealed trait Json

case class JsonObject(get: Map[String, Json]) extends Json

case class JsonString(get: String) extends Json

case class JsonNumber(get: Int) extends Json

case object JsonNull extends Json

trait JsonWriter[A] {
  def writer(value: A): Json
}

final case class Person(name: String, email: String)

object JsonWriterInstances {
  implicit val StringWriter: JsonWriter[String] = new JsonWriter[String] {
    def writer(value: String): Json = {
      JsonString(value)
    }
  }
}
