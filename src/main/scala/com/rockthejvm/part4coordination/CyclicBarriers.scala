package com.rockthejvm.part4coordination

import cats.effect.kernel.{Deferred, Ref}
import cats.effect.{IO, IOApp}
import cats.effect.std.CyclicBarrier

import scala.util.Random
import scala.concurrent.duration._
import com.rockthejvm.utils._
import cats.syntax.parallel._

object CyclicBarriers extends IOApp.Simple {

  /*
    A cyclic barrier is a coordination primitive that
    - is initialized with a count
    - has a single API: await

    A cyclic barrier will (semantically) block all fibers calling its await() method until we have exactly N fibers waiting,
      at which point the barrier will unblock all fibers and reset to its original state.
    Any further fiber will again block until we have exactly N fibers waiting.
    ...
    And so on.
   */

  // example: signing up for a social network just about to be launched
  def createUser(id: Int, barrier: CBarrier): IO[Unit] = for {
    _ <- IO.sleep((Random.nextDouble * 500).toInt.millis)
    _ <- IO(s"[user $id] Just heard there's a new social network - signing up for the waitlist...").debug
    _ <- IO.sleep((Random.nextDouble * 1500).toInt.millis)
    _ <- IO(s"[user $id] On the waitlist now, can't wait!").debug
    _ <- barrier.await // block the fiber when there are exactly N users waiting
    _ <- IO(s"[user $id] OMG this is so cool!").debug
  } yield ()

  def openNetwork(): IO[Unit] = for {
    _ <- IO("[announcer] The Rock the JVM social network is up for registration! Launching when we have 10 users!").debug
    barrier <- CBarrier(10)
    _ <- (1 to 20).toList.parTraverse(id => createUser(id, barrier))
  } yield ()

  /**
   * Exercise: Implement your own CB with Ref + Deferred. Ignore cancellation effects.
   * Test: use your CBarrier instead of CyclicBarrier[IO].
   */

  override def run = openNetwork()
}

abstract class CBarrier {
  def await: IO[Unit]
}

object CBarrier {
  case class State(nWaiting: Int, signal: Deferred[IO, Unit])

  def apply(count: Int): IO[CBarrier] = for {
    signal <- Deferred[IO, Unit]
    state <- Ref[IO].of(State(count, signal))
  } yield new CBarrier {
    override def await = Deferred[IO, Unit].flatMap { newSignal =>
      state.modify {
        case State(1, signal) => State(count, newSignal) -> signal.complete(()).void
        case State(n, signal) => State(n - 1, signal) -> signal.get
      }.flatten
    }
  }
}