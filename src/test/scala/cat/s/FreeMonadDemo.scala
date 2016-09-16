package cat.s

import cats.{Monad, RecursiveTailRecM}
import cats.free.Free


object FreeMonadDemo {

  sealed trait Interaction[A]

  case class Ask(prompt: String) extends Interaction[String]

  case class Tell(message: String) extends Interaction[Unit]

}

class FreeMonadDemo extends ynfrastructure.Spec {

  "it must work" in {
    //based on Composable application architecture with reasonably priced monads
    //https://www.youtube.com/watch?v=M258zVn4m2M

    import FreeMonadDemo._
    import cats.free

    type FreeInteraction[A] = Free[Interaction, A]

    import cats.free.Free.liftF

    def ask(prompt: String): FreeInteraction[String] = liftF(Ask(prompt))
    def tell(message: String): FreeInteraction[Unit] = liftF(Tell(message))

    val prog: FreeInteraction[Unit] = for {
      first <- ask("What is your first name?")
      last <- ask("What is your last name?")
      _ <- tell(s"Hello $first $last!")
    } yield ()

    import cats.{Id, ~>}
    //WTF naturaltransformation

    //interpreter for production
    object Console extends (Interaction ~> Id) {
      override def apply[A](fa: Interaction[A]): Id[A] = fa match {
        case Ask(s) => println(s); scala.io.StdIn.readLine()
        case Tell(m) => println(m);
      }
    }

    //    prog.foldMap(Console)

    //interpreter for testing
    type Tester[A] = Map[String, String] => (List[String], A)

    object Test extends (Interaction ~> Tester) {
      override def apply[A](fa: Interaction[A]): Tester[A] = fa match {
        case Ask(prompt) => m => (List(), m(prompt))
        case Tell(message) => m => (List(message), ())
      }
    }

    implicit val testerMonad = new Monad[Tester] {

      override def flatMap[A, B](fa: Tester[A])(f: (A) => Tester[B]): Tester[B] = m => {
        val (o1, a) = fa(m)
        val (o2, b) = f(a)(m)
        (o1 ++ o2, b)
      }

      override def tailRecM[A, B](a: A)(f: (A) => Tester[Either[A, B]]): Tester[B] = defaultTailRecM(a)(f)

      override def pure[A](a: A): Tester[A] = _ => (List(), a)
    }

    implicit val recursiveTailRecMTester: RecursiveTailRecM[Tester] = RecursiveTailRecM.create[Tester]

    val tester: Tester[Unit] = prog.foldMap(Test)

    val io: Map[String, String] = Map("What is your first name?" -> "Harry", "What is your last name?" -> "Potem")
    tester(io) mustBe(List("Hello Harry Potem!"), ())
  }
}
