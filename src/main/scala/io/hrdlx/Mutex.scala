package io.hrdlx

import cats.effect.IO
import scala.collection.immutable.Queue
import cats.effect.kernel.Deferred
import cats.effect.IOApp
import cats.syntax.parallel._
import cats.effect.std.Random
import cats.effect.implicits._

trait Mutex {
  def acquire: IO[Unit]
  def release: IO[Unit]
}

object Mutex {
  case class State(locked: Boolean, waiting: Queue[Deferred[IO, Unit]])
  def create: IO[Mutex] = IO
    .ref(State(false, Queue()))
    .map { stateRef =>
      new Mutex {
        override def acquire: IO[Unit] = IO
          .deferred[Unit]
          .flatMap(signal =>
            stateRef.modify {
              case State(false, _) =>
                State(true, Queue()) -> IO.unit
              case State(true, queue) =>
                State(true, queue :+ signal) -> signal.get
            }.flatten
          )

        override def release: IO[Unit] =
          stateRef.modify {
            case State(false, queue) => State(false, queue) -> IO.unit
            case State(true, queue) =>
              if (queue.isEmpty) State(false, queue) -> IO.unit
              else {
                val (sygnal, rest) = queue.dequeue
                State(true, rest) -> sygnal.complete(()).void
              }
          }.flatten
      }
    }
}


object MutexExample extends IOApp.Simple {
  override def run: IO[Unit] = Mutex.create.flatMap{mutex => 
    (1 to 10).toList.parTraverse(n => for {
      _ <- IO.println(s"[$n task] initialized")
      _ <- mutex.acquire
      _ <- IO.println(s"[$n task] started.")
      random <- Random.scalaUtilRandom[IO]
      result <- random.nextIntBounded(100)
      _ <- IO.println(s"[$n task] finished. result: $result")
      _ <- mutex.release
    } yield()).void
  }
}
