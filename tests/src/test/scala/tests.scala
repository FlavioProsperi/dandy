package tests

import dandy._
import cats._

case class Person(name: String)

object Greets {
  implicit object showPerson extends Show[Person] {
    def show(p: Person) = s"""Person named "${p.name}""""
  }
  implicit val greetPerson = Greet.instantiate[Person] {
    def greet(p: Person) = s"Hello, ${implicitly[Show[Person]].show(p)}!"
  }
}

@typeclass class Multiplier[A[_]: Functor]
