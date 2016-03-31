package dandy

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.annotation.StaticAnnotation
import scala.reflect.runtime.universe.{ WeakTypeTag => RTWeakTypeTag }

class typeclass extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TypeclassMacros.impl
}

class TypeclassMacros(val c: whitebox.Context) {
  import c.universe._

  def lastImpl[Cons[_], Arg](implicit wtc: WeakTypeTag[Cons[Arg]], wta: WeakTypeTag[Arg]): c.Expr[Cons[Arg]] = {
    val TypeRef(_, impl, _) = wtc.tpe.typeConstructor
    c.Expr[Cons[Arg]](q"new ${impl.asClass}[$wta]")
  }

  trait Renamer[N <: Name] { def rename(name: N, f: String => String): N }
  object Renamer {
    implicit object TypeRenamer extends Renamer[TypeName] {
      def rename(name: TypeName, f: String => String) =
        TypeName(f(name.toString))
    }
    implicit object TermRenamer extends Renamer[TermName] {
      def rename(name: TermName, f: String => String) =
        TermName(f(name.toString))
    }
  }

  implicit class Rename[N <: Name: Renamer](name: N) {
    def rename(f: String => String) =
      implicitly[Renamer[N]].rename(name, f)
  }

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
    def unapply(trees: List[Tree]): Option[List[Either[Tree, Subject]]] =
      Some(trees.map {
        case Subject(subj) => Right(subj)
        case other => Left(other)
      })
  }

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
    def _1 = this
    def _2 = defitparams
    def _3 = itparams
    def _4 = body

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

  def impl(annottees: c.Expr[Any]*) = {
    annottees.map(_.tree) match {
      case q"$mods trait $tc[..${ Subjects(subjects) }] { ..$body }" :: rest =>
        val params = subjects.map(_.fold(identity, _.orig))
        val implicits = subjects.flatMap(_.fold(_ => Nil, _.implicits()))
        val companionProto = if (rest.isEmpty) q"object ${tc.toTermName}" :: Nil else rest
        val companion = companionProto.collect {
          case q"object $name { ..$protos }" =>
            val instances = protos.collect {
              case InstanceDecl(instance, defitparams, itparams, instanceBody) =>
                val impl =
                  c.freshName(tc.rename(_ + s"$$${itparams.mkString(",")}"))
                val instanceImplicits =
                  instance.subjectImplicits ::: instance.implicits(subjects)
                List(
                  q"""
                    private class $impl[..${defitparams.map(_.arg)}](
                      implicit ..$instanceImplicits
                    ) extends $tc[..$itparams] { ..$instanceBody }
                  """,
                  q"""
                    implicit def ${c.freshName(tc.toTermName.rename("instance$" + _))}[
                      ..${defitparams.map(_.arg)}
                    ](
                      implicit ..$instanceImplicits
                    ): $tc[..$itparams] =
                      new $impl[..${defitparams.map(_.name)}]
                  """
                )
              case other => other :: Nil
            }
            val definst_tparams: List[Tree] = subjects.collect {
              case Right(subj) => subj.orig
              case Left(arg) => arg
            }
            val definst_targs: List[TypeName] = subjects.collect {
              case Right(subj) => subj.subject
              case Left(TypeDefName(arg)) => arg
            }
            val alphas = definst_targs.map(tn => c.freshName(tn.rename("$alpha" + _)))
            val Some(levels) =
              alphas.reverse.zipWithIndex.foldLeft(Option.empty[Tree]) {
                case (None, (alpha @ TypeName(alphaName), 0)) =>
                  Some(q"""
                    class ${TypeName(s"${alphaName}__$alphaName")}[$alpha]
                    extends $tc[..${alphas}] {
                    }
                  """)
                case (Some(next), (alpha @ TypeName(alphaName), idx)) =>
                  val q"class ${ last @ TypeName(lastName) }[${ TypeDefName(alphaLast) }] extends $parentLast { ..$bodylast }" = next
                  val step: List[Tree] =
                    idx match {
                      case 1 =>
                        List(
                          q"""
                            private def step0[$alphaLast]: $last[$alphaLast] =
                              macro TypeclassMacros.lastImpl[${TypeName(lastName)}, $alphaLast]
                          """,
                          q"def step[$alphaLast]: $tc[..$alphas] = step0[$alphaLast]"
                        )
                      case _ =>
                        q"def step[$alphaLast] = new $last[$alphaLast]" :: Nil
                    }
                  Some(q"""
                    class ${TypeName(s"${alphaName}__$alphaName")}[$alpha] {
                      $next
                      ..$step
                    }
                  """)
              }
            val def_instance =
              q"""
              def instance[..${definst_tparams}](body: Any)(implicit ..$implicits): $tc[..${definst_targs}] = ???
            """
            q"""
              object $name {
                ..${instances.flatten}
                $def_instance
                $levels
                def step[${alphas.head}] = new ${TypeName(s"${alphas.head}__${alphas.head}")}[${alphas.head}]
              }
            """
        }
        val result = q"""
          abstract class $tc[..$params](implicit ..$implicits) { ..$body }
          ..$companion
        """
        println(showCode(result))
        result
    }
  }

  val WC = TypeDef(
    Modifiers(Flag.PARAM),
    typeNames.WILDCARD,
    List(),
    TypeBoundsTree(EmptyTree, EmptyTree)
  )
}
