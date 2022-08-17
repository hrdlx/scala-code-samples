package io.hrdlx

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.Functor
import cats.syntax.all._

object State {
  trait Counter[F[_]] {
    def inc: F[Unit]
    def get: F[Int]
  }

  object Counter {
    def create[F[_]: Functor: Ref.Make]: F[Counter[F]] =
      Ref.of[F, Int](0).map { ref =>
        new Counter[F] {
          override def get: F[Int] = ref.get
          override def inc: F[Unit] = ref.update(_+1)
        }
      }
  }
}
