package mk.ukim.finki.skit.property

case class Person(name: String, age: Int) {
  val isAdult: Boolean = age >= 18
}
