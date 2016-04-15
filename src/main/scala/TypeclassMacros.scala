package dandy

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.annotation.StaticAnnotation
import scala.reflect.runtime.universe.{ WeakTypeTag => RTWeakTypeTag }

class typeclass(debug: Boolean = false) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TypeclassMacros.impl
}

class TypeclassMacros(val c: whitebox.Context) extends Renamers with Subjects with Utils with TypeclassModel with TypeclassCompanionModel {
  import c.universe._

  def impl(annottees: c.Expr[Any]*) = {
    val debug = c.prefix.tree match {
      case q"new typeclass(..$params)" =>
        params.collect {
          case q"debug = $d" => d
          case q"$d" => d
        }.headOption.map {
          case q"false" => false
          case q"true" => true
        }.getOrElse(false)
      case q"new typeclass()" => false
      case _ => sys.error(showCode(c.prefix.tree))
    }
    annottees.map(_.tree) match {
      case Typeclass(tc) =>
        val result = q"""
          ${tc.definition.tree}
          ..${tc.companion.tree(debug)}
        """
        if (debug) println(result)
        result
    }
  }
}
