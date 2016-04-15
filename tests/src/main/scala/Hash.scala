package tests

import dandy._

@typeclass trait Hash[A] {
  def hash(a: A): Int
}

object Hash {
  instance[A][A] {
    def hash(a: A) = a.hashCode
  }
}
