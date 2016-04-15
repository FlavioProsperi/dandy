package dandy

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.annotation.StaticAnnotation
import scala.reflect.runtime.universe.{ WeakTypeTag => RTWeakTypeTag }

class typeclass extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TypeclassMacros.impl
}

class TypeclassMacros(val c: whitebox.Context) extends Renamers with Subjects with Utils with TypeclassModel with TypeclassCompanionModel {
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

  def impl(annottees: c.Expr[Any]*) = {
    annottees.map(_.tree) match {
      case Typeclass(tc) =>
        val result = q"""
          ${tc.definition.tree}
          ..${tc.companion.tree}
        """
        println(showCode(result))
        result
    }
  }
}
