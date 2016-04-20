<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [Dandy: a classy Scala syntax for type classes](#dandy-a-classy-scala-syntax-for-type-classes)
  - [Why do type classes need first-class syntax?](#why-do-type-classes-need-first-class-syntax)
    - [A reduction in boilerplate](#a-reduction-in-boilerplate)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Dandy: a classy Scala syntax for type classes

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
also add an instance `Foo2[Int]` with as little boilerplate as possible:

```scala
object Two { // plz fix https://github.com/tpolecat/tut/issues/62
  import dandy._

  // context bounds on type param `A` require that a `Show[A]` exist
  @typeclass class Foo2[A:Show] {
    def foo2(a: A): String = s"foo2: ${Show[A].show(a)}"
  }
  object Foo2 {
    // define instance `Show[Int]`
    instance[Int] {
      def foo2(a: Int) =
        // `Show[Int]` is implicitly available in this scope
        s"an int: -> ${Show[Int].show(a)} <-"
    }
  }

  def go() {
    import cats.implicits._ // we will need `Show` instances
    println(implicitly[Foo2[Int]].foo2(42))
  }
}
// defined object Two

Two.go()
// an int: -> 42 <-
```
