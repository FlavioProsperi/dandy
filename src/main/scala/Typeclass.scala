package dandy

trait TypeclassModel {
  self: TypeclassMacros =>

  import c.universe._

  class Typeclass(
    val definition: TypeclassDef,
    val companion: TypeclassCompanion
  )

  class TypeclassDef(
      val mods: Modifiers,
      val name: TypeName,
      val subjects: List[Either[TypeDef, Subject]],
      val body: List[Tree]
  ) {
    val typeParams = subjects.map(_.fold(identity, _.orig))
    val superImplicits = subjects.flatMap(_.fold(_ => Nil, _.implicits()))
    val tree = q"""
      abstract class $name[..$typeParams](
        implicit ..$superImplicits
      ) { ..$body }
    """
  }

  object Typeclass {
    def unapply(trees: List[Tree]): Option[Typeclass] = trees match {
      case q"$mods trait $tc[..${ Subjects(subjects) }] { ..$body }" :: rest =>
        val typeclass = new TypeclassDef(mods, tc, subjects, body)
        Some(new Typeclass(typeclass, new TypeclassCompanion(typeclass, rest)))
      case _ => None
    }
  }
}
