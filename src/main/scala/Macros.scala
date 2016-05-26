package dandy

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.annotation.StaticAnnotation
import scala.reflect.runtime.universe.{ WeakTypeTag => RTWeakTypeTag }

class typeclass extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TypeclassMacros.impl
}

private[dandy] case class Config(
  debug: Boolean = false,
  apply: Boolean = true,
  export: Boolean = true
)

class TypeclassMacros(val c: whitebox.Context)
    extends Renamers
    with Subjects
    with Utils
    with TypeclassModel
    with CompanionModel {
  import c.universe._

  val config = c.prefix.tree match {
    case q"new typeclass()" => Config()
    case q"new typeclass(..$params)" =>
      params.foldLeft(Config()) {
        case (c, q"debug = ${ Literal(Constant(debug: Boolean)) }") =>
          c.copy(debug = debug)
        case (c, q"apply = ${ Literal(Constant(apply: Boolean)) }") =>
          c.copy(apply = apply)
        case (c, q"export = ${ Literal(Constant(export: Boolean)) }") =>
          c.copy(export = export)
      }
  }

  def impl(annottees: c.Expr[Any]*) = {
    annottees.map(_.tree) match {
      case Typeclass(tc) =>
        val result = q"""
          ${tc.definition.tree}
          ..${tc.companion.tree(config)}
        """
        if (config.debug) println(result)
        result
    }
  }
}
