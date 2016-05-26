package tests

import dandy._
import cats._

@typeclass class MkString[A, FO[_]: Foldable] {
  def mkString(foa: FO[A], separator: String): String
}

object MkString {
  def instance[A: Show, FO[_]] {
    def mkString(foa: FO[A], separator: String) = {
      import cats.kernel.std._
      Foldable[FO].foldLeft(foa, "") {
        case ("", elem) => Show[A].show(elem)
        case (acc, elem) => s"${acc}${separator}${Show[A].show(elem)}"
      }
    }
  }
}
