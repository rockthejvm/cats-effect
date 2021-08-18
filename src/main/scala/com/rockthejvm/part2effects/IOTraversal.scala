package com.rockthejvm.part2effects

import cats.effect.{IO, IOApp}

import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple {

  import scala.concurrent.ExecutionContext.Implicits.global

  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  val workLoad: List[String] = List("I quite like CE", "Scala is great", "looking forward to some awesome stuff")

  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workLoad.map(heavyComputation)
    // Future[List[Int]] would be hard to obtain
    futures.foreach(_.foreach(println))
  }

  import cats.Traverse
  import cats.instances.list._
  val listTraverse = Traverse[List]

  def traverseFutures(): Unit = {
    // traverse
    val singleFuture: Future[List[Int]] = listTraverse.traverse(workLoad)(heavyComputation)
    // ^^ this stores ALL the results
    singleFuture.foreach(println)
  }

  import com.rockthejvm.utils._

  // traverse for IO
  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.debug

  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO)

  // parallel traversal
  import cats.syntax.parallel._ // parTraverse extension method
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)

  /**
   * Exercises
   */
  // hint: use Traverse API
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listTraverse.traverse(listOfIOs)(x => x)

  // hard version
  def sequence_v2[F[_] : Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(wrapperOfIOs)(x => x)

  // parallel version
  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.parTraverse(x => x)

  // hard version
  def parSequence_v2[F[_] : Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    wrapperOfIOs.parTraverse(x => x)

  // existing sequence API
  val singleIO_v2: IO[List[Int]] = listTraverse.sequence(ios)

  // parallel sequencing
  val parallelSingleIO_v2: IO[List[Int]] = parSequence(ios) // from the exercise
  val parallelSingleIO_v3: IO[List[Int]] = ios.parSequence // extension method from the Parallel syntax package

  override def run =
    parallelSingleIO_v3.map(_.sum).debug.void
}
