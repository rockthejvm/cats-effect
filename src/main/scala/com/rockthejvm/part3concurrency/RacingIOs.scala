package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp}

import scala.concurrent.duration.{FiniteDuration, *}

object RacingIOs extends IOApp.Simple {

  import com.rockthejvm.utils.*

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (
      IO(s"starting computation: $value").debug >>
      IO.sleep(duration) >>
      IO(s"computation for $value: done") >>
      IO(value)
    ).onCancel(IO(s"computation CANCELED for $value").debug.void)

  def testRace() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)
    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    /*
      - both IOs run on separate fibers
      - the first one to finish will complete the result
      - the loser will be canceled
     */

    first.flatMap {
      case Left(mol) => IO(s"Meaning of life won: $mol")
      case Right(lang) => IO(s"Fav language won: $lang")
    }
  }

  def testRacePair() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)
    val raceResult: IO[Either[
      (Outcome[IO, Throwable, Int], Fiber[IO, Throwable, String]), // (winner result, loser fiber)
      (Fiber[IO, Throwable, Int], Outcome[IO, Throwable, String])  // (loser fiber, winner result)
    ]] = IO.racePair(meaningOfLife, favLang)

    raceResult.flatMap {
      case Left((outMol, fibLang)) => fibLang.cancel >> IO("MOL won").debug >> IO(outMol).debug
      case Right((fibMol, outLang)) => fibMol.cancel >> IO("Language won").debug >> IO(outLang).debug
    }
  }

  /**
   * Exercises:
   * 1 - implement a timeout pattern with race
   * 2 - a method to return a LOSING effect from a race (hint: use racePair)
   * 3 - implement race in terms of racePair
   */
  // 1
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val timeoutEffect = IO.sleep(duration)
    val result = IO.race(io, timeoutEffect)

    result.flatMap {
      case Left(v) => IO(v)
      case Right(_) => IO.raiseError(new RuntimeException("Computation timed out."))
    }
  }

  val importantTask = IO.sleep(2.seconds) >> IO(42).debug
  val testTimeout = timeout(importantTask, 1.seconds)
  val testTimeout_v2 = importantTask.timeout(1.seconds)

  // 2
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((_, fibB)) => fibB.join.flatMap {
        case Succeeded(resultEffect) => resultEffect.map(result => Right(result))
        case Errored(e) => IO.raiseError(e)
        case Canceled() => IO.raiseError(new RuntimeException("Loser canceled."))
      }
      case Right((fibA, _)) => fibA.join.flatMap {
        case Succeeded(resultEffect) => resultEffect.map(result => Left(result))
        case Errored(e) => IO.raiseError(e)
        case Canceled() => IO.raiseError(new RuntimeException("Loser canceled."))
      }
    }

  // 3
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) => outA match {
        case Succeeded(effectA) => fibB.cancel >> effectA.map(a => Left(a))
        case Errored(e) => fibB.cancel >> IO.raiseError(e)
        case Canceled() => fibB.join.flatMap {
          case Succeeded(effectB) => effectB.map(b => Right(b))
          case Errored(e) => IO.raiseError(e)
          case Canceled() => IO.raiseError(new RuntimeException("Both computations canceled."))
        }
      }
      case Right((fibA, outB)) => outB match {
        case Succeeded(effectB) => fibA.cancel >> effectB.map(b => Right(b))
        case Errored(e) => fibA.cancel >> IO.raiseError(e)
        case Canceled() => fibA.join.flatMap {
          case Succeeded(effectA) => effectA.map(a => Left(a))
          case Errored(e) => IO.raiseError(e)
          case Canceled() => IO.raiseError(new RuntimeException("Both computations canceled."))
        }
      }
    }

  override def run = testRace().debug.void
}
