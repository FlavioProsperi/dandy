package dandy

trait Utils {
  self: TypeclassMacros =>

  import c.universe._

  object TypeDefName {
    def unapply(t: Tree): Option[TypeName] = t match {
      case tn @ TypeName(_) => Some(tn)
      case Ident(tn @ TypeName(_)) => Some(tn)
      case ExistentialTypeTree(AppliedTypeTree(Ident(tn @ TypeName(_)), _), _) => Some(tn)
      case TypeDef(_, tn @ TypeName(_), _, _) => Some(tn)
      case AppliedTypeTree(Ident(tn @ TypeName(_)), _) => Some(tn)
      case _ => None
    }
  }

  val WC = TypeDef(
    Modifiers(Flag.PARAM),
    typeNames.WILDCARD,
    List(),
    TypeBoundsTree(EmptyTree, EmptyTree)
  )
}
