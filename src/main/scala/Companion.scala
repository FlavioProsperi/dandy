package dandy

trait CompanionModel {
  self: TypeclassMacros =>

  import c.universe._

  sealed trait Instance {
    def args: List[TypeName]
    def body: List[Tree]

    protected def impl(typeclass: TypeclassDef) =
      c.freshName(typeclass.name.toTermName.rename(_ + s"$$${args.mkString(",")}"))

    protected def instanceTypeArgs: List[TypeDef] = Nil
    protected def additionalEvidence: List[ValDef] = Nil

    def render(typeclass: TypeclassDef) = {
      val instanceName = impl(typeclass)
      val instanceClass = instanceName.toTypeName.rename("_" + _)
      val evidence: List[ValDef] = (typeclass.subjects.zip(args).flatMap {
        case (subject, arg) => subject.evidence(arg)
      }) ::: additionalEvidence
      q"""
        class $instanceClass[
          ..$instanceTypeArgs
        ](
          implicit ..$evidence
        ) extends ${typeclass.name}[..$args] {
          ..${body.map(OverrideDef[c.type](c)(_))}
        }
      """ ::
        q"""
        implicit def $instanceName[
          ..$instanceTypeArgs
        ](
          implicit ..$evidence
        ) = new $instanceClass
      """ :: Nil
    }
  }

  case class ConstantInstance(
    args: List[TypeName],
    body: List[Tree]
  ) extends Instance

  case class ParametricInstance(
      subjects: List[Subject],
      body: List[Tree]
  ) extends Instance {
    val args = subjects.map(_.typeName)
    protected override def instanceTypeArgs = subjects.map(_.typeDef)
    protected override def additionalEvidence = subjects.flatMap(_.evidence())
  }

  object Instance {
    def unapply(tree: Tree): Option[Instance] = tree match {
      case q"instance[..$tparams]({ ..$body })" =>
        Some(ConstantInstance(
          args = tparams.collect { case TypeDefName(tn) => tn },
          body = body
        ))
      case DefDef(_, TermName("instance"), tparams, _, _, body) =>
        (tree :: Nil) match {
          case Subjects(SubjectsAndBody(subjects, _)) =>
            Some(ParametricInstance(
              subjects = subjects,
              body = body.children
            ))
        }
    }
  }

  class InstantiateParam(subject: Subject) {
    val typeDef = subject.typeDef
    val TypeDef(_, name, args, _) = typeDef
    val wt = name match {
      case TypeName(name) => c.freshName(TermName(s"wt$name"))
    }
    val evidence = q"""implicit val $wt: c.universe.WeakTypeTag[
      $name[..${List.fill(args.size)(typeNames.WILDCARD)}]
    ]"""
  }

  class TypeclassCompanion(typeclass: TypeclassDef, protos: List[Tree]) {
    val declarations = protos match {
      case q"object ${ _ } { ..$body }" :: Nil => body
      case _ => Nil
    }
    val members = declarations.collect {
      case Instance(instance) => instance.render(typeclass)
      case other => other :: Nil
    }
    val instantiateParams = typeclass.subjects.map(new InstantiateParam(_))
    def tree(debug: Boolean) = q"""
      object ${typeclass.name.toTermName} {
        ..${members.flatten}
        def instantiate[
          ..${instantiateParams.map(_.typeDef)}
        ](body: Any*): ${typeclass.name}[..${instantiateParams.map(_.name)}] =
          macro instantiateImpl[..${instantiateParams.map(_.name)}]
        def instantiateImpl[
          ..${instantiateParams.map(_.typeDef)}
        ](
          c: scala.reflect.macros.whitebox.Context
        )(
          body: c.Expr[Any]*
        )(
          implicit ..${instantiateParams.map(_.evidence)}
        ): c.Expr[${typeclass.name}[..${instantiateParams.map(_.name)}]] = {
          import c.universe._
          val parts: Seq[Tree] = body.map(_.tree).flatMap {
            case Block(statements, exprs) => exprs :: statements
            case statement => statement :: Nil
          }.map(dandy.OverrideDef[c.type](c)(_)).map(c.untypecheck(_))
          val tree = StringContext("new ", "[..", "] { ..", " }").q(
            TypeName(${typeclass.name.toString}),
            ${instantiateParams.map(_.wt)}, parts
          )
          if ($debug) println(tree)
          c.Expr[${typeclass.name}[..${instantiateParams.map(_.name)}]](tree)
        }
      }
    """
  }
}
object OverrideDef {
  import scala.reflect.macros.whitebox
  def apply[C <: whitebox.Context](c: C)(tree: c.universe.Tree) = {
    import c.universe._
    tree match {
      case DefDef(Modifiers(flags, privateWithin, annotations), name, tparams, vparamss, tpt, rhs) =>
        DefDef(Modifiers(flags | Flag.OVERRIDE, privateWithin, annotations), name, tparams, vparamss, tpt, rhs)
      case x => x
    }
  }
}
