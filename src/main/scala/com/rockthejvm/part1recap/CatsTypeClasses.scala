package com.rockthejvm.part1recap

object CatsTypeClasses {

  /*
    - applicative
    - functor
    - flatMap
    - monad
    - applicativeError/monadError
    - traverse
   */

  // functor - "mappable" data structures
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list.*
  val listFunctor = Functor[List]

  // generalizable "mapping" APIs
  def increment[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(container)(_ + 1)

  import cats.syntax.functor.*
  def increment_v2[F[_] : Functor](container: F[Int]): F[Int] =
    container.map(_ + 1)

  // applicative - the ability to "wrap" types
  trait MyApplicative[F[_]] extends MyFunctor[F] {
    def pure[A](value: A): F[A]
  }

  import cats.Applicative
  val applicativeList = Applicative[List]
  val aSimpleList: List[Int] = applicativeList.pure(43)
  import cats.syntax.applicative.* // import the pure extension method
  val aSimpleList_v2: List[Int] = 43.pure[List]

  // FlatMap - ability to chain multiple wrapper computations
  trait MyFlatMap[F[_]] extends MyFunctor[F] {
    def flatMap[A, B](container: F[A])(f: A => F[B]): F[B]
  }

  import cats.FlatMap
  val flatMapList = FlatMap[List]
  import cats.syntax.flatMap.* // flatMap extension method
  def crossProduct[F[_] : FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  // Monad - applicative + flatMap
  trait MyMonad[F[_]] extends MyApplicative[F] with MyFlatMap[F] {
    override def map[A, B](initialValue: F[A])(f: A => B): F[B] =
      flatMap(initialValue)(a => pure(f(a)))
  }

  import cats.Monad
  val monadList = Monad[List]
  def crossProduct_v2[F[_] : Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  /*
         Functor -> FlatMap -->
                 \             \
                    Applicative -> Monad
   */

  // error-like type classes
  trait MyApplicativeError[F[_], E] extends MyApplicative[F] {
    def raiseError[A](e: E): F[A]
  }

  import cats.ApplicativeError
  type ErrorOr[A] = Either[String, A]
  val appErrorEither = ApplicativeError[ErrorOr, String]
  val desirableValue: ErrorOr[Int] = appErrorEither.pure(42)
  val failedValue: ErrorOr[Int] = appErrorEither.raiseError("Something failed")
  import cats.syntax.applicativeError.* // raiseError extension method
  val failedValue_v2: ErrorOr[Int] = "Something failed".raiseError[ErrorOr, Int]

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F]
  import cats.MonadError
  val monadErrorEither = MonadError[ErrorOr, String]

  // traverse
  trait MyTraverse[F[_]] extends MyFunctor[F] {
    def traverse[G[_], A, B](container: F[A])(f: A => G[B]): G[F[B]]
  }

  // turn nested wrappers inside out
  val listOfOptions: List[Option[Int]] = List(Some(1), Some(2), Some(43))
  import cats.Traverse
  val listTraverse = Traverse[List]
  val optionList: Option[List[Int]] = listTraverse.traverse(List(1,2,3))(x => Option(x))
  import cats.syntax.traverse.*
  val optionList_v2: Option[List[Int]] = List(1,2,3).traverse(x => Option(x))

  /*
    Big(ger) type class hierarchy in Cats:

      Semigroup -> Monoid

      Semigroupal -> Apply -> FlatMap -->
                  /        \             \
      Functor --->          Applicative -> Monad ---------> MonadError
         |                              \                 /
         |                               ApplicativeError
         |
         ----> Traverse
   */
  def main(args: Array[String]): Unit = {

  }
}
