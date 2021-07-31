package com.rockthejvm.part4coordination

import cats.effect
import cats.effect.kernel.Deferred
import cats.effect.std.CountDownLatch
import cats.effect.{IO, IOApp, Ref, Resource}

import scala.concurrent.duration._
import com.rockthejvm.utils._
import cats.syntax.parallel._
import cats.syntax.traverse._

import java.io.{File, FileWriter}
import scala.io.Source
import scala.util.Random

object CountdownLatches extends IOApp.Simple {

  /*
    CDLatches are a coordination primitive initialized with a count.
    All fibers calling await() on the CDLatch are (semantically) blocked.
    When the internal count of the latch reaches 0 (via release() calls from other fibers), all waiting fibers are unblocked.
   */

  def announcer(latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO("Starting race shortly...").debug >> IO.sleep(2.seconds)
    _ <- IO("5...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("4...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("3...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("2...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("1...").debug >> IO.sleep(1.second)
    _ <- latch.release // gun firing
    _ <- IO("GO GO GO!").debug
  } yield ()

  def createRunner(id: Int, latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO(s"[runner $id] waiting for signal...").debug
    _ <- latch.await // block this fiber until the count reaches 0
    _ <- IO(s"[runner $id] RUNNING!").debug
  } yield ()

  def sprint(): IO[Unit] = for {
    latch <- CountDownLatch[IO](5)
    announcerFib <- announcer(latch).start
    _ <- (1 to 10).toList.parTraverse(id => createRunner(id, latch))
    _ <- announcerFib.join
  } yield ()


  /**
   * Exercise: simulate a file downloader on multiple threads
   */
  object FileServer {
    val fileChunksList = Array(
      "I love Scala.",
      "Cats Effect seems quite fun.",
      "Never would I have thought I would do low-level concurrency WITH pure FP."
    )

    def getNumChunks: IO[Int] = IO(fileChunksList.length)
    def getFileChunk(n: Int): IO[String] = IO(fileChunksList(n))
  }

  def writeToFile(path: String, contents: String): IO[Unit] = {
    val fileResource = Resource.make(IO(new FileWriter(new File(path))))(writer => IO(writer.close()))
    fileResource.use { writer =>
      IO(writer.write(contents))
    }
  }

  def appendFileContents(fromPath: String, toPath: String): IO[Unit] = {
    val compositeResource = for {
      reader <- Resource.make(IO(Source.fromFile(fromPath)))(source => IO(source.close()))
      writer <- Resource.make(IO(new FileWriter(new File(toPath), true)))(writer => IO(writer.close()))
    } yield (reader, writer)

    compositeResource.use {
      case (reader, writer) => IO(reader.getLines().foreach(writer.write))
    }
  }

  def createFileDownloaderTask(id: Int, latch: CDLatch, filename: String, destFolder: String): IO[Unit] = for {
    _ <- IO(s"[task $id] downloading chunk...").debug
    _ <- IO.sleep((Random.nextDouble * 1000).toInt.millis)
    chunk <- FileServer.getFileChunk(id)
    _ <- writeToFile(s"$destFolder/$filename.part$id", chunk)
    _ <- IO(s"[task $id] chunk download complete").debug
    _ <- latch.release
  } yield ()

  /*
    - call file server API and get the number of chunks (n)
    - start a CDLatch
    - start n fibers which download a chunk of the file (use the file server's download chunk API)
    - block on the latch until each task has finished
    - after all chunks are done, stitch the files together under the same file on disk
   */
  def downloadFile(filename: String, destFolder: String): IO[Unit] = for {
    n <- FileServer.getNumChunks
    latch <- CDLatch(n)
    _ <- IO(s"Download started on $n fibers.").debug
    _ <- (0 until n).toList.parTraverse(id => createFileDownloaderTask(id, latch, filename, destFolder))
    _ <- latch.await
    _ <- (0 until n).toList.traverse(id => appendFileContents(s"$destFolder/$filename.part$id", s"$destFolder/$filename"))
  } yield ()

  override def run = downloadFile("myScalafile.txt", "cats-effect/src/main/resources")
}

/**
 * Exercise: implement your own CDLatch with Ref and Deferred.
 */

abstract class CDLatch {
  def await: IO[Unit]
  def release: IO[Unit]
}

object CDLatch {
  sealed trait State
  case object Done extends State
  case class Live(remainingCount: Int, signal: Deferred[IO, Unit]) extends State

  def apply(count: Int): IO[CDLatch] = for {
    signal <- Deferred[IO, Unit]
    state <- Ref[IO].of[State](Live(count, signal))
  } yield new CDLatch {

    override def await = state.get.flatMap { s =>
      if (s == Done) IO.unit // continue, the latch is dead
      else signal.get // block here
    }

    override def release = state.modify {
      case Done => Done -> IO.unit
      case Live(1, signal) => Done -> signal.complete(()).void
      case Live(n, signal) => Live(n - 1, signal) -> IO.unit
    }.flatten.uncancelable
  }
}