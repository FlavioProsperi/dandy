package dandy

trait Subjects {
  self: TypeclassMacros =>

  import c.universe._

  class Subject(val subject: TypeName, val orig: Tree, bounds: Tree) {
    override def toString = subject.toString
    val supers: List[Tree] = {
      def supers0(t: Tree): List[Tree] =
        t match {
          case tq"::[$a, $b]" => a :: supers0(b)
          case x => x :: Nil
        }
      supers0(bounds)
    }
    def implicits(arg: TypeName = subject) =
      supers.zipWithIndex.map {
        case (tq"${ superTC @ TypeDefName(superName) }", idx) =>
          val valname = c.freshName(superName.toTermName.rename(_ + s"$$$arg"))
          q"protected val $valname: $superTC[$arg]"
      }
  }

  object Subject {
    def unapply(t: Tree): Option[Subject] = t match {
      case TypeDef(mods, name, tparams, TypeBoundsTree(_, hi)) if hi.nonEmpty =>
        Some(new Subject(name, TypeDef(mods, name, tparams, TypeBoundsTree(q"", q"")), hi))
      case tq"<<:[${ lhs @ TypeDefName(name) }, $supers]" =>
        Some(new Subject(name, lhs, supers))
      case _ => None
    }
  }

  object Subjects {
    def unapply(trees: List[Tree]): Option[List[Either[TypeDef, Subject]]] =
      Some(trees.map {
        case Subject(subj) => Right(subj)
        case typeDef: TypeDef => Left(typeDef)
      })
  }
}
