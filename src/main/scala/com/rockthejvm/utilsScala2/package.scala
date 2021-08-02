package com.rockthejvm

import cats.effect.IO

package object utilsScala2 {
  implicit class DebugWrapper[A](io: IO[A]) {
    def debug: IO[A] = for {
      a <- io
      t = Thread.currentThread().getName
      _ = println(s"[$t] $a")
    } yield a
  }
}
