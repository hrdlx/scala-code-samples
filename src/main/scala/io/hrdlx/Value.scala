package io.hrdlx

import io.estatico.newtype.macros._
import eu.timepit.refined.types.string
import eu.timepit.refined.api.RefinedTypeOps
import cats.syntax.all._
import cats.data.NonEmptyList
import cats.data.EitherNel

//vanila
case class Value private (value: String) extends AnyVal
object Value {
  // smart constructor with cats guard
  def create(value: String): Option[Value] =
    (value.nonEmpty).guard[Option].as(Value(value))
}

package object types {
  // with newtype and refined validation
  type ValueR = string.NonEmptyString
  object ValueR extends RefinedTypeOps[ValueR, String]
  @newtype case class Value(value: ValueR)

  type AnotherValueR = string.NonEmptyString
  object AnotherValueR extends RefinedTypeOps[AnotherValueR, String]
  @newtype case class AnotherValue(value: AnotherValueR)

  case class Consumer(value: Value, anotherValue: AnotherValue)

  // usage
  val result: EitherNel[String, Consumer] = (
    ValueR.from("value").toEitherNel.map(Value.apply),
    AnotherValueR.from("anotherValue").toEitherNel.map(AnotherValue.apply)
  ).parMapN(Consumer.apply)

  // using helper
  import io.hrdlx.utils._

  val anotherResult: EitherNel[String, Consumer]=  (
    validate[Value]("value"),
    validate[AnotherValue]("anotehrValue")
  ).parMapN(Consumer.apply)
}
