package io.hrdlx

import cats.effect.IO
import scala.collection.immutable.Queue
import cats.effect.kernel.Deferred
import cats.effect.IOApp
import cats.syntax.parallel._
import cats.effect.std.Random
import cats.effect.implicits._

import scala.concurrent.duration._

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
            IO.uncancelable { pool =>
              stateRef.modify {
                case State(false, _) =>
                  State(true, Queue()) -> IO.unit
                case State(true, queue) =>
                  State(true, queue :+ signal) -> pool(signal.get)
                    .onCancel { stateRef.modify {
                      case State(locked, queue) =>
                        State(locked, queue.filterNot(_ == signal)) -> release
                    }.flatten}
              }.flatten
            }
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
  val mutextTest = Mutex.create.flatMap { mutex =>
    (1 to 10).toList
      .parTraverse(n =>
        for {
          _ <- IO.println(s"[$n task] initialized")
          _ <- mutex.acquire
          _ <- IO.println(s"[$n task] started.")
          random <- Random.scalaUtilRandom[IO]
          result <- random.nextIntBounded(100)
          _ <- IO.println(s"[$n task] finished. result: $result")
          _ <- mutex.release
        } yield ()
      )
      .void
  }

  def simpleTask(mutex: Mutex, id: String, io: IO[Unit]): IO[Unit] =
    mutex.acquire >>
      IO.println(s"[task $id] start") >>
      io >>
      IO.println(s"[task $id] end") >>
      mutex.release

  val cancelationTest = for {
    mutex <- Mutex.create
    fb1 <- simpleTask(mutex, "A", IO.sleep(1.second)).start
    fb2 <- simpleTask(mutex, "B", IO.sleep(1.second) >> IO.canceled).start
    fb3 <- simpleTask(mutex, "C", IO.sleep(1.second)).start
    _ <- fb1.join
    _ <- fb2.join
    _ <- fb3.join
  } yield ()

  override def run: IO[Unit] = cancelationTest
}
