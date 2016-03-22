package dandy

import cats._

@typeclass trait Greet[A <: Show] {
  def greet(a: A): String
}

object Greet {
  instance[String] {
    def greet(s: String) = s"This is a string: $s"
  }
  instance[T][T] {
    def greet(t: T) = s"This is something else: ${implicitly[Show[T]].show(t)}"
  }
}
