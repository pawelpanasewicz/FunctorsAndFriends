package exercise


/**
  *
  * Write core method signatures and inheritance relations between core type classes.
  *
  * 1. Functor (map)
  * 2. Apply (ap)
  * 3. Applicative (pure)
  * 4. Monad (flatMap)
  *
  * Use it as kata.
  */
class E1_Signatures {

  object possibleSolution {

    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A => B): F[B]
    }

    trait Apply[F[_]] extends Functor[F] {
      def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
    }

    trait Applicative[F[_]] extends Apply[F] {
      def pure[A](a: A): F[A]
    }

    trait Monad[F[_]] extends Applicative[F] {
      def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
    }
  }

}
