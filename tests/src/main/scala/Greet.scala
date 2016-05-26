package tests

import dandy._
import cats._

@typeclass class Greet[A: Show] {
  def greet(a: A): String
}

object Greet {
  def instance[T: Hash] {
    def greet(t: T) = s"This is something else: ${Show[T].show(t)} @ ${implicitly[Hash[T]].hash(t)}"
  }

  instance[String] {
    def greet(s: String) =
      s"This is a string: ${Show[String].show(s)}"
  }

  instance[Int] {
    def greet(s: Int) =
      s"This is a int: ${Show[Int].show(s)}"
  }
}
