package dandy

import shapeless._

@typeclass trait Single[A]
@typeclass trait Double[A, B]
@typeclass trait Triple[A, B, C]
@typeclass trait Quadruple[A, B, C, D]
