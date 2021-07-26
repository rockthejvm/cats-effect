package com.rockthejvm.part2effects

import cats.effect.{ExitCode, IO, IOApp}

import scala.io.StdIn

object IOApps {
  val program = for {
    line <- IO(StdIn.readLine())
    _ <- IO(println(s"You've just written: $line"))
  } yield ()
}

object TestApp {
  import IOApps._

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    program.unsafeRunSync()
  }
}

object FirstCEApp extends IOApp {
  import IOApps._

  override def run(args: List[String]) =
    program.as(ExitCode.Success)
}

object MySimpleApp extends IOApp.Simple {
  import IOApps._

  override def run = program
}

