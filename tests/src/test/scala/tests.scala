package tests

import dandy._
import cats._
import cats.implicits._
import org.specs2._
import org.specs2.execute._, Typecheck._
import org.specs2.matcher.TypecheckMatchers._

class DandySpec extends Specification {
  def is = s2"""
    Greet type class must greet
      Strings ${greets.string}
      Ints ${greets.int}
      BigInts ${greets.bigInt}
      Persons ${greets.person}
    MkString type class must join
      Options ${mkStrings.options}
      Lists ${mkStrings.lists}
      Vectors ${mkStrings.vectors}
      two-of-s ${mkStrings.twoOfs}
    Hash type class must hash
      strings ${hashes.strings}
  """

  case class TwoOf[A](one: A, two: A)
  implicit object twoOfFoldable extends Foldable[TwoOf] {
    def foldLeft[A, B](fa: TwoOf[A], b: B)(f: (B, A) => B): B =
      Foldable[List].foldLeft(fa.one :: fa.two :: Nil, b)(f)
    def foldRight[A, B](fa: TwoOf[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      Foldable[List].foldRight(fa.one :: fa.two :: Nil, lb)(f)
  }

  case class Person(name: String)
  implicit object showPerson extends Show[Person] {
    def show(p: Person) = s"""Person named "${p.name}""""
  }
  implicit val greetPerson = Greet.instantiate[Person] {
    def greet(p: Person) = s"Hello, ${implicitly[Show[Person]].show(p)}!"
  }

  object greets {
    import Greet._
    def string = greet("something") must_== "This is a string: something"
    def int = greet(42) must_== "This is a int: 42"
    def bigInt = greet(BigInt(42)) must_== "This is something else: 42 @ 42"
    def person = greet(Person("yeltsin")) must_== """Hello, Person named "yeltsin"!"""
  }

  object mkStrings {
    import MkString._
    def options = mkString(Option(1), ",") must_== "1"
    def lists = mkString(1 :: 2 :: 3 :: Nil, ",") must_== "1,2,3"
    def vectors = mkString(Vector(1, 2, 3), ",") must_== "1,2,3"
    def twoOfs = mkString(TwoOf(1, 2), ",") must_== "1,2"
    def persons = mkString(TwoOf(Person("lenin"), Person("trotsky")), ",") must_== "lenin,trotsky"
  }

  object hashes {
    import Hash._
    def strings = hash("foo") must_== 101574
  }
}
