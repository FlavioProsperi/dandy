# Dandy: a classy Scala syntax for type classes

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [Why do type classes need first-class syntax?](#why-do-type-classes-need-first-class-syntax)
  - [A reduction in boilerplate](#a-reduction-in-boilerplate)
  - [Latent instantiation](#latent-instantiation)
- [Where do you want to go today?](#where-do-you-want-to-go-today)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

Type classes are only incidentally tractable in Scala. We have just enough
low-level facilities to define type classes and instances, but the language
doesn't provide any syntax to express these concepts at a higher level and
without so much boilerplate.

## Why do type classes need first-class syntax?

Consider the following example. Here we define a type class `Foo` that requires
its members to be also in `Show`. In Haskell this is trivially accomplished by
way of type class inheritance:

```haskell
class Show a => Foo a where
  foo :: a -> [Char]
  foo a = "foo: " ++ (show a)

instance Foo [Char] where

main = putStrLn $ foo "hello"
-- foo: hello
```

How can this be replicated in Scala? Tediously!

```scala
import cats.Show
// import cats.Show

class Foo1[A:Show] {
  def foo1(a: A): String = s"foo1: ${Show[A].show(a)}"
}
// defined class Foo1

implicit def anyFoo1[A:Show] = new Foo1[A]
// anyFoo1: [A](implicit evidence$1: cats.Show[A])Foo1[A]

def foo1[A:Foo1](a: A) = implicitly[Foo1[A]].foo1(a)
// foo1: [A](a: A)(implicit evidence$1: Foo1[A])String

{
  import cats.implicits._
  println(foo1("hello"))
}
// foo1: hello
```

That wasn't too bad, but what happens when we need to override the default
implementation of `foo1`? This isn't so pretty:

```scala
class IntFoo1 extends Foo1[Int] {
  override def foo1(a: Int) = s"an int: -> $a <-"
}
// <console>:15: error: could not find implicit value for evidence parameter of type cats.Show[Int]
//        class IntFoo1 extends Foo1[Int] {
//                              ^
```

We're missing the `:Show` implicit... Which can be resolved with yet more
boilerplate:

```scala
class IntFoo1(implicit S: Show[Int]) extends Foo1[Int] {
  override def foo1(a: Int) = s"an int: -> $a <-"
}
// defined class IntFoo1
```

This is a blatant violation of the DRY principle: each new instance of `Foo1[A]`
will need to regurgitate the need for a `Show[A]`. This is too much typing!

### A reduction in boilerplate

Dandy includes a macro annotation - `@typeclass` - that generates a type class
definition based on the annotated structure. This is easily explained using an
example. Here we're going to define `Foo2[A]` just as above, then proceed to
also add an instance `Foo2[Int]` with as little boilerplate as possible. We
will also define a catch-all instance `Foo2[A]`.

```scala
// plz fix https://github.com/tpolecat/tut/issues/62
// :paste mode would be hella nice here
object foo2Example {
  import dandy._

  // context bounds on `A` require that a `Show[A]` exist for all instances of
  // `Show` defined here or elsewhere
  @typeclass class Foo2[A:Show] {
    def foo2(a: A): String
  }
  object Foo2 {
    // define instance `Show[Int]`
    instance[Int] {
      // `Show[Int]` is implicitly available in this scope
      def foo2(a: Int) = s"an int: -> ${Show[Int].show(a)} <-"
    }

    // define a catch-all instance
    def instance[A] {
      def foo2(a: A) = s"something else: => ${Show[A].show(a)} <="
    }
  }

  import cats.implicits._ // we will need `Show` instances
  println(implicitly[Foo2[Int]].foo2(42))
  println(implicitly[Foo2[String]].foo2("42"))
}
// defined object foo2Example

foo2Example
// an int: -> 42 <-
// something else: => 42 <=
// res3: foo2Example.type = foo2Example$@40981dff
```

### Latent instantiation

Unsealed type classes allow users of a library to create new instances anywhere
and everywhere. There isn't a universally accepted syntax for this; all of the
following have been spotted in commercial and FOSS code:

```scala
object foo3Example {
  trait Foo3[A] { /* ... */ }
  implicit object one extends Foo3[String]
  implicit val two = new Foo3[String] {}
  implicit val three: Foo3[String] = new Foo3[String] {}
  implicit def four[A] = new Foo3[A] {}
  implicit def five[A]: Foo3[A] = new Foo3[A] {}
  // ... is there more? there's always room for moar...
}
// defined object foo3Example

foo3Example
// res4: foo3Example.type = foo3Example$@6bfe6f73
```

Dandy provides a cleaner syntax for creating type class instances outside of
the type class companion. First, let's define the type class:

```scala
object foo4 {
  import dandy._
  @typeclass class Foo4[A:Show] {
    def foo4(a: A): String
  }
}
// defined object foo4
```

Next, we define some instances outside of the companion:

```scala
import foo4._
// import foo4._

import cats.implicits._
// import cats.implicits._

implicit val foo4Int = Foo4.instantiate[Int] {
  def foo4(i: Int) = Show[Int].show(i)
}
// foo4Int: foo4.Foo4[Int] = $anon$1@75071c60

println(implicitly[Foo4[Int]].foo4(42))
// 42
```

This works for any types; let's define one on the spot:

```scala
case class Person(name: String)
// defined class Person

implicit object showPerson extends Show[Person] {
  def show(p: Person) = s"a person named '${p.name}'"
}
// defined object showPerson

implicit val foo4Person = Foo4.instantiate[Person] {
  def foo4(p: Person) = Show[Person].show(p)
}
// foo4Person: foo4.Foo4[Person] = $anon$1@11377e3f

println(implicitly[Foo4[Person]].foo4(Person(name = "Albert Einstein")))
// a person named 'Albert Einstein'
```

We can also instantiate catch-all instances:

```scala
implicit def anyFoo[A:Show] = Foo4.instantiate[A] {
  def foo4(a: A) = s"something: ${Show[A].show(a)}"
}
// anyFoo: [A](implicit evidence$1: cats.Show[A])foo4.Foo4[A]

case class N(n: Int)
// defined class N

implicit object showN extends Show[N] {
  def show(n: N) = n.toString
}
// defined object showN

println(implicitly[Foo4[N]].foo4(N(42)))
// something: N(42)
```

## Where do you want to go today?

Dandy is a work in progress. Its ideology is similar in some ways to
[simulacrum](https://github.com/mpilquist/simulacrum) and
[typeclassic](https://github.com/typelevel/typeclassic), but...
* Dandy sets its sights first and foremost on reducing boilerplate to the bare
  minimum. Simulacrum and Typeclassic offer a few other features which Dandy
  currently classifies as non-goals, although these will be easy to add if
  necessary.
* The code does some potentially dodgy things. For example, the generated
  companion's `instantiate` method is itself a def macro generated by the
  `@typeclass` macro annotation... whoa.
* Dandy's design and implementation likely suffer from an extension of the
  author's shortsightedness in terms of how people normally define and use type
  classes.
* The macros make assumptions about AST structure that aren't enforced by means
  other than `scala.MatchError`. This aspect will be improved if the code
  proceeds beyond its current purely experimental stage.
* It's possible that no one in Scala land gives a cack about Haskell-style type
  class syntax, or type class inheritance. `¯\_(ツ)_/¯`
