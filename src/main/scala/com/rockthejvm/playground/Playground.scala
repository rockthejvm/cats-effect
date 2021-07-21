package com.rockthejvm.playground

import cats.effect.{IO, IOApp}

object Playground extends IOApp.Simple {

  override def run: IO[Unit] =
    IO.println("Learning Cats Effect 3! Looking forward to it...")
}