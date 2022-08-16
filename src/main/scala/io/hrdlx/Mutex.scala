package io.hrdlx

import cats.effect.IO
import scala.collection.immutable.Queue
import cats.effect.kernel.Deferred

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
