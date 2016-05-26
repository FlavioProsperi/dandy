package dandy

trait Subjects {
  self: TypeclassMacros =>

  import c.universe._

  sealed trait Subject {
    def typeDef: TypeDef
    val TypeDef(_, typeName, _, _) = typeDef
    def evidence(arg: TypeName = typeName): List[ValDef]
  }
  case class PlainParam(typeDef: TypeDef) extends Subject {
    def evidence(arg: TypeName = typeName) = Nil
  }
  case class WithSupertypes(typeDef: TypeDef, supers: List[TypeName]) extends Subject {
    def evidence(arg: TypeName = typeName) = supers.map {
      case superType =>
        val valname = c.freshName(superType.toTermName.rename(_ + s"$$$arg"))
        q"protected val $valname: $superType[$arg]"
    }
  }

  class ExtractSubjects(tparams: List[TypeDef], params: List[Tree]) {
    object ArgForParam {
      def unapply(arg: Tree): Option[TypeDef] = arg match {
        case Ident(arg @ TypeName(_)) =>
          tparams.find {
            case TypeDef(_, tparamName, _, _) if tparamName == arg => true
            case _ => false
          }
        case _ => None
      }
    }

    object WithSupertypesParam {
      private def likely_superval(mods: Modifiers) = {
        import Flag._
        mods.hasFlag(SYNTHETIC) && mods.hasFlag(IMPLICIT)
      }
      def unapply(tree: Tree): Option[(TypeDef, TypeName)] = tree match {
        case q"$valmods val ${ _ }: ${ Ident(supertypeclass @ TypeName(_)) }[${ ArgForParam(tparam) }] = ${ _ }" if likely_superval(valmods) => Some(tparam -> supertypeclass)
        case x =>
          None
      }
    }
  }

  case class SubjectsAndBody(subjects: List[Subject], body: List[Tree] = Nil)

  object Subjects {
    private def go(tparams: List[TypeDef], body: List[Tree]): SubjectsAndBody = {
      object es extends ExtractSubjects(tparams, body)
      import es._
      val (withSuperTypes, bodyRest) = {
        val (withSuperTypes0, bodyRest0) = body.partition {
          case WithSupertypesParam(_, _) => true
          case _ => false
        }
        (
          withSuperTypes0
          .flatMap(WithSupertypesParam.unapply(_))
          .groupBy(_._1)
          .mapValues(_.map(_._2))
          .map { case (typeDef, supers) => WithSupertypes(typeDef, supers) },
          bodyRest0.filter {
            case DefDef(_, TermName("<init>"), _, _, _, _) => false
            case _ => true
          }
        )
      }
      val subjects: List[Subject] = tparams.map(p => p -> withSuperTypes.find(_.typeDef == p)).map {
        case (param, None) => PlainParam(param)
        case (_, Some(withSupers)) => withSupers
      }
      SubjectsAndBody(subjects, bodyRest)
    }
    def unapply(trees: List[Tree]): Option[SubjectsAndBody] = trees match {
      case ClassDef(_, _, tparams, Template(_, _, body)) :: rest =>
        Some(go(tparams, body))
      case DefDef(_, TermName("instance"), tparams, paramss, _, _) :: Nil =>
        Some(go(tparams, paramss match {
          case params :: Nil => params
          case _ => Nil
        }))
    }
  }
}
