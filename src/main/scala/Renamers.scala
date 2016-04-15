package dandy

trait Renamers {
  self: TypeclassMacros =>

  import c.universe._

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
}
