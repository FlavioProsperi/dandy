package dandy

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.annotation.StaticAnnotation

class typeclass extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TypeclassMacros.impl
}

class TypeclassMacros(val c: whitebox.Context) {
  import c.universe._

  class Subject(val subject: TypeName, val orig: Tree, bounds: Tree) {
    val supers: List[Tree] = {
      def supers0(t: Tree): List[Tree] =
        t match {
          case tq"::[$a, $b]" => a :: supers0(b)
          case x => x :: Nil
        }
      supers0(bounds)
    }
    def implicits(arg: TypeName = subject) = supers.zipWithIndex.map {
      case (tq"$superTC", idx) =>
        q"protected val ${TermName(c.freshName())}: $superTC[$arg]"
    }
  }

  object Subject {
    def unapply(t: Tree): Option[Subject] = t match {
      case TypeDef(mods, name, tparams, TypeBoundsTree(_, hi)) if hi.nonEmpty =>
        Some(new Subject(name, TypeDef(mods, name, tparams, TypeBoundsTree(q"", q"")), hi))
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

  object InstanceDecl {
    def unapply(t: Tree): Option[(List[Tree], List[Tree], List[Tree])] = t match {
      case q"instance[..$itparams]({ ..$anonbody })" => Some((Nil, itparams, anonbody))
      case q"instance[..$defitparams][..$itparams]({ ..$anonbody })" =>
        val fixed = defitparams.map {
          case ExistentialTypeTree(AppliedTypeTree(Ident(name @ TypeName(_)), wildcards), _) =>
            q"type $name[..${List.fill(wildcards.size)(WC)}]"
          case Ident(name @ TypeName(_)) => q"type $name"
        }
        Some((fixed, itparams, anonbody))
      case _ => None
    }
  }

  object TypeDefName {
    def unapply(t: Tree): Option[TypeName] = t match {
      case Ident(tn @ TypeName(_)) => Some(tn)
      case ExistentialTypeTree(AppliedTypeTree(Ident(tn @ TypeName(_)), _), _) => Some(tn)
      case TypeDef(_, tn @ TypeName(_), _, _) => Some(tn)
      case _ => None
    }
  }

  def impl(annottees: c.Expr[Any]*) = {
    annottees.map(_.tree) match {
      case q"$mods trait $tc[..${ Subjects(subjects) }] { ..$body }" :: rest =>
        val params = subjects.map(_.fold(identity, _.orig))
        val implicits = subjects.flatMap(_.fold(_ => Nil, _.implicits()))
        val companion = rest.collect {
          case q"object $name { ..$protos }" =>
            val instances = protos.collect {
              case InstanceDecl(defitparams, itparams, anonbody) =>
                val iimplicits = itparams
                  .zip(subjects.collect {
                    case Right(subject) => subject.implicits _
                  })
                  .flatMap {
                    case (TypeDefName(itparam), f) => f(itparam)
                  }
                val impl = TypeName(c.freshName())
                val implTypeArgs = defitparams.collect {
                  case TypeDefName(tn) => tn
                }
                List(
                  q"""
                  class $impl[..$defitparams](implicit ..$iimplicits)
                  extends $tc[..$itparams] { ..$anonbody }
                """,
                  q"""
                  implicit def ${TermName(c.freshName())}[..$defitparams](implicit ..$iimplicits) = new $impl[..$implTypeArgs]
                """
                )
              case other => other :: Nil
            }
            q""" object $name { ..${instances.flatten} } """
        }
        val result = q"""
          abstract class $tc[..$params](implicit ..$implicits) { ..$body }
          ..$companion
        """
        println(showCode(result))
        result
    }
  }

  val WC = TypeDef(Modifiers(Flag.PARAM), typeNames.WILDCARD, List(), TypeBoundsTree(EmptyTree, EmptyTree))
}
