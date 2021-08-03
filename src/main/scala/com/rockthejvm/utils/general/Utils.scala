package com.rockthejvm.utils.general

import cats.Functor
import cats.syntax.functor.*

import cats.effect.MonadCancel
import scala.concurrent.duration.FiniteDuration

extension [F[_], A](fa: F[A]) {
  def debug(using functor: Functor[F]): F[A] = fa.map { a =>
    val t = Thread.currentThread().getName
    println(s"[$t] $a")
    a
  }
}

def unsafeSleep[F[_], E](duration: FiniteDuration)(using mc: MonadCancel[F, E]): F[Unit] =
  mc.pure(Thread.sleep(duration.toMillis))
