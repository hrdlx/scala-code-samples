package io.hrdlx

import cats.data.EitherNel
import io.estatico.newtype.Coercible
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.Validate
import eu.timepit.refined.refineV
import cats.syntax.all._
import io.estatico.newtype.ops._

package object utils {

  class NewTypeRefinedOps[T] {
    def apply[R, S](raw: R)(implicit
        c: Coercible[Refined[R, S], T],
        v: Validate[R, S]
    ): EitherNel[String, T] = refineV[S](raw).toEitherNel.map(_.coerce[T])
  }

  def validate[T]: NewTypeRefinedOps[T] = new NewTypeRefinedOps[T]
}
