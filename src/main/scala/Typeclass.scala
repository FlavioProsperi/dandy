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
      val subjects: List[Subject],
      val body: List[Tree]
  ) {
    val tree = q"""
      abstract class $name[..${subjects.map(_.typeDef)}](
        implicit ..${subjects.flatMap(_.evidence())}
      ) { ..$body }
    """
  }

  object Typeclass {
    def unapply(trees: List[Tree]): Option[Typeclass] = trees match {
      case ClassDef(mods, name, params, Template(_, _, body)) :: rest =>
        trees match {
          case Subjects(SubjectsAndBody(subjects, bodyStatements)) =>
            val typeclass = new TypeclassDef(mods, name, subjects, bodyStatements)
            Some(new Typeclass(typeclass, new TypeclassCompanion(typeclass, rest)))
          case _ => None
        }
      case _ => None
    }
  }
}
