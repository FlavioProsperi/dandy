package dandy

trait TypeclassCompanionModel {
  self: TypeclassMacros =>

  import c.universe._

  case class Defitparam(
    orig: Tree,
    name: TypeName,
    arg: TypeDef,
    subject: Option[Subject] = None
  )

  object Defitparam {
    def unapply(tree: Tree): Option[Defitparam] = tree match {
      case tq"<<:[${ Defitparam(p) }, $supers]" =>
        val Subject(s) = tree
        Some(p.copy(subject = Some(s)))
      case ExistentialTypeTree(AppliedTypeTree(Ident(name @ TypeName(_)), wildcards), _) =>
        Some(new Defitparam(
          orig = tree,
          name = name,
          arg = q"type $name[..${List.fill(wildcards.size)(WC)}]"
        ))
      case Ident(name @ TypeName(_)) =>
        Some(new Defitparam(
          name = name,
          orig = tree,
          arg = q"type $name"
        ))
      case AppliedTypeTree(Ident(name @ TypeName(_)), args) =>
        val argNames = args.collect {
          case Ident(arg @ TypeName(_)) => q"type $arg"
        }
        Some(new Defitparam(
          name = name,
          orig = tree,
          arg = q"type $name[..$argNames]"
        ))
    }
  }

  class InstanceDecl(
      val defitparams: List[Defitparam],
      val itparams: List[Tree],
      val body: List[Tree]
  ) {
    val isEmpty = false
    def get = this

    val subjectImplicits: List[ValDef] = defitparams.collect {
      case Defitparam(_, _, _, Some(subject)) =>
        subject.implicits()
    }.flatten

    def implicits(subjects: List[Either[Tree, Subject]]): List[ValDef] =
      itparams
        .zip(subjects.collect {
          case Right(subject) => subject.implicits _
        })
        .flatMap {
          case (TypeDefName(itparam), f) => f(itparam)
        }

    def render(typeclass: TypeclassDef) = {
      val impl =
        c.freshName(typeclass.name.rename(_ + s"$$${itparams.mkString(",")}"))
      val instanceImplicits =
        subjectImplicits ::: implicits(typeclass.subjects)
      List(
        q"""
            private class $impl[..${defitparams.map(_.arg)}](
              implicit ..$instanceImplicits
            ) extends ${typeclass.name}[..$itparams] { ..$body }
          """,
        q"""
          implicit def ${c.freshName(typeclass.name.toTermName.rename("instance$" + _))}[
            ..${defitparams.map(_.arg)}
            ](
              implicit ..$instanceImplicits
            ): ${typeclass.name}[..$itparams] =
              new $impl[..${defitparams.map(_.name)}]
              """
      )
    }
  }

  object InstanceDecl {
    def unapply(t: Tree): Option[InstanceDecl] = t match {
      case q"instance[..$itparams]({ ..$body })" =>
        Some(new InstanceDecl(Nil, itparams, body))
      case q"instance[..$defitparams][..$itparams]({ ..$body })" =>
        Some(new InstanceDecl(
          defitparams.collect { case Defitparam(p) => p },
          itparams,
          body
        ))
      case _ => None
    }
  }

  class InstantiateParam(
      subject: Either[TypeDef, Subject]
  ) {
    val typeDef = subject.fold(identity, _.orig)
    val TypeDef(_, name, args, _) = typeDef
    val wt = name match {
      case TypeName(name) => c.freshName(TermName(s"wt$name"))
    }
    val evidence = q"""implicit val $wt: c.universe.WeakTypeTag[
      $name[..${List.fill(args.size)(typeNames.WILDCARD)}]
    ]"""
  }

  class TypeclassCompanion(
      typeclass: TypeclassDef,
      protos: List[Tree]
  ) {
    val declarations = protos match {
      case q"object ${ _ } { ..$body }" :: Nil => body
      case _ => Nil
    }
    val members = declarations.collect {
      case InstanceDecl(instance) => instance.render(typeclass)
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
          }.map {
            case DefDef(Modifiers(flags, privateWithin, annotations), name, tparams, vparamss, tpt, rhs) =>
              DefDef(Modifiers(flags | Flag.OVERRIDE, privateWithin, annotations), name, tparams, vparamss, tpt, rhs)
            case x => x
          }.map(c.untypecheck(_))
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
