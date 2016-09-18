package cat.s

import cats.Monad
import cats.kernel.Eq
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}

class CustomMonads extends ynfrastructure.DisciplineSpec {

  //How man can use discipline library to test that this instance of Monad obeys Monad Laws?

  test("monad must obey monad lows") {

    //given some monad instance for FUN[A] type
    type FUN[A] = Map[String, String] => (List[String], A)
    val funMonad: Monad[FUN] = new Monad[FUN] {
      override def flatMap[A, B](fa: FUN[A])(f: (A) => FUN[B]): FUN[B] = m => {
        val (list1, a1) = fa(m)
        val (list2, a2) = f(a1)(m)
        (list1 ++ list2, a2)
      }
      override def pure[A](x: A): FUN[A] = m => (Nil, x)
      override def tailRecM[A, B](a: A)(f: (A) => FUN[Either[A, B]]): FUN[B] = defaultTailRecM(a)(f)
    }

    //here is discipline scrap showing how to test it

    //this is example inputs for testing equality between two instances of Fun[T]

    //how to test if functions are equal?
    //thanks god we are in tests so little estimation is allowed here...
    //Two functions are equal when for the same inputs they generate the same outputs


    implicit def funEq[T: Eq]: Eq[FUN[T]] = {
      val sampleInput: Map[String, String] = {
        def genMap: Gen[Map[String, String]] = for {
          size <- Gen.size
          keys <- Gen.containerOfN[List, String](size, Arbitrary.arbitrary[String])
          values <- Gen.containerOfN[List, String](size, Arbitrary.arbitrary[String])
        } yield keys.zip(values).toMap

        genMap(Gen.Parameters.default.withSize(10), Seed.apply(123L)).get
      }

      Eq.instance[FUN[T]] ((f1, f2) => f1(sampleInput) == f2(sampleInput))
    }

    import cats.kernel.instances.int._
    import cats.kernel.instances.tuple._
    import cats.laws.discipline.MonadTests


    //checkAll("Int", MonadTests[FUN](funMonad).monad[Int, Int, Int])
    //TODO uncomment and finish it - http://stackoverflow.com/questions/39561525/how-to-test-monad-instance-using-discipline
    //Error:(53, 53) could not find implicit value for parameter iso: cats.laws.discipline.CartesianTests.Isomorphisms[[A]scala.collection.immutable.Map[String,String] => (List[String], A)]

  }
}
