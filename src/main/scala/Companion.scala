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
        ): ${typeclass.name}[..$args] = new $instanceClass
      """ :: Nil
    }
  }

  case class ConstantInstance(
    args: List[TypeName],
    body: List[Tree]
  ) extends Instance

  case class ValueParametricInstance(
      args: List[TypeName],
      instanceImplicits: List[Tree],
      body: List[Tree]
  ) extends Instance {
    protected override def additionalEvidence =
      instanceImplicits.collect {
        case vd: ValDef => vd
        case Typed(Ident(ten @ TermName(_)), Ident(tyn @ TypeName(_))) => q"val $ten: $tyn"
      }
  }

  case class TypeParametricInstance(
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
      case q"instance[..$tparams](..$instanceImplicits)({ ..$body })" =>
        Some(ValueParametricInstance(
          args = tparams.collect { case TypeDefName(tn) => tn },
          instanceImplicits = instanceImplicits,
          body = body
        ))
      case DefDef(_, TermName("instance"), tparams, _, _, body) =>
        (tree :: Nil) match {
          case Subjects(SubjectsAndBody(subjects, _)) =>
            Some(TypeParametricInstance(
              subjects = subjects,
              body = body.children
            ))
        }
      case _ => None
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
    val members = (protos match {
      case q"object ${ _ } { ..$body }" :: Nil => body
      case _ => Nil
    }).collect {
      case Instance(instance) => instance.render(typeclass)
      case other => other :: Nil
    }
    object instantiate {
      val params = typeclass.subjects.map(new InstantiateParam(_))
      val defs = params.map(_.typeDef)
      val names = params.map(_.name)
    }

    val apply = if (config.apply) {
      val ev = c.freshName(TermName("ev"))
      q"""
        // Generated apply to ease retrieval of an implicitly available
        // instance of type class.
        def apply[
          ..${instantiate.defs}
        ](
          implicit $ev: ${typeclass.name}[..${instantiate.names}]
        ): ${typeclass.name}[..${instantiate.names}] = $ev
      """ :: Nil
    } else Nil

    val exports = if (config.export) {
      def is_public(mods: Modifiers) =
        !mods.hasFlag(Flag.PRIVATE) && !mods.hasFlag(Flag.PROTECTED)
      val methods = typeclass.body.collect {
        case DefDef(mods, name, tparams, paramss, ret, _) if is_public(mods) =>
          val export_tparams = tparams ::: instantiate.defs
          val export_paramss = paramss :::
            List(q"implicit val ${c.freshName(TermName("ev"))}: ${typeclass.name}[..${instantiate.names}]") ::
            Nil
          q"""
            def $name[ ..$export_tparams ]( ...$export_paramss): $ret =
              apply[..${instantiate.names}].${name}(...${paramss.map(_.map(_.name))})
          """
      }
      methods
    } else Nil

    def tree(config: Config) = q"""
      object ${typeclass.name.toTermName} {
        // Companion members include:
        // - Generated instances.
        // - Other stuff caller has put inside the companion that Dandy
        // ignores.
        ..${members.flatten}

        ..$apply
        ..$exports

        // Macro def shorthand for building an instance outside of the
        // companion. Yes, this macro generates a macro...
        def instantiate[
          ..${instantiate.defs}
        ](body: Any*): ${typeclass.name}[..${instantiate.names}] =
          macro instantiateImpl[..${instantiate.names}]

        // This macro splices a provided "body" (which can be anything, but is
        // hopefully enough to implement the type class) inside an instance
        // creation call. Takes care of type parameters, implicits, etc...
        // Less boilerplate is more.
        def instantiateImpl[
          ..${instantiate.defs}
        ](
          c: scala.reflect.macros.whitebox.Context
        )(
          body: c.Expr[Any]*
        )(
          implicit ..${instantiate.params.map(_.evidence)}
        ): c.Expr[${typeclass.name}[..${instantiate.names}]] = {
          import c.universe._
          val parts: Seq[Tree] = body.map(_.tree).flatMap {
            case Block(statements, exprs) => exprs :: statements
            case statement => statement :: Nil
          }.map(dandy.OverrideDef[c.type](c)(_)).map(c.untypecheck(_))
          val tree = StringContext("new ", "[..", "] { ..", " }").q(
            TypeName(${typeclass.name.toString}),
            ${instantiate.params.map(_.wt)}, parts
          )
          if (${config.debug}) println(tree)
          c.Expr[${typeclass.name}[..${instantiate.names}]](tree)
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
