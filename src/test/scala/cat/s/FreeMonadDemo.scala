package cat.s

import cats.{Monad, RecursiveTailRecM}
import cats.free.Free

object FreeMonadDemo {

  sealed trait Interaction[A]

  case class Ask(prompt: String) extends Interaction[String]

  case class Tell(message: String) extends Interaction[Unit]

}

class FreeMonadDemo extends ynfrastructure.Spec {

  "ubiquitous tutorial" in {
    //based on Composable application architecture with reasonably priced monads
    //https://www.youtube.com/watch?v=M258zVn4m2M

    //domain of our program is described as algebraic data structures
    //they must be in seperate object otherwise scalac will have problems to compile it
    import FreeMonadDemo._

    //Let's create some type alias describing our domain
    //Thanks to that we can put our domain object inside Free Monad and take advantage of 'monadic composition'
    type FreeInteraction[A] = Free[Interaction, A]

    //instead of working on pure Interaction types, we want to lift them into monad.
    //This can be achieved using liftF function which simply puts some value into Suspend case class
    import cats.free.Free.liftF

    //thanks to liftF we can create some handy routines.
    //They are often called the DSL and can be used to create our programs
    def ask(prompt: String): FreeInteraction[String] = liftF(Ask(prompt))
    def tell(message: String): FreeInteraction[Unit] = liftF(Tell(message))

    //ok. We have DSL for creating program. Let's use it and write some hello world program.
    //It will ask for name, surname and will print welcome message.
    //Note that prog is type of FreeInteraction[Unit] (the alias we defined earlier)
    //The prog is only the description of our program. If you expand it you will
    //see that it is just a data structure, which can be freely interpreted. 
    //This is why Free Monad is called the Free 
    val prog: FreeInteraction[Unit] = for {
      first <- ask("What is your first name?")
      last <- ask("What is your last name?")
      _ <- tell(s"Hello $first $last!")
    } yield ()

    //Let's create some concrete interpretation of prog (data structure representing program).

    //First we need the NaturalTransformation which simply represents the function of type F[_] => G[_]
    //Note that Id has the shape op G[_] and is often chosen here
    import cats.{Id, ~>}

    //This is the one possible interpreter of programs
    object ConsoleInterpreter extends (Interaction ~> Id) {
      //Above line can be written as well as
      //object ConsoleInterpreter extends NaturalTransformation[Interaction, Id]

      //It has only one method, which tells what to do (how to handle) the Interaction instance:
      override def apply[A](fa: Interaction[A]): Id[A] = fa match {
        case Ask(s) => println(s); scala.io.StdIn.readLine()
        case Tell(m) => println(m);
      }
    }

    //And this is how you can run the program. Just call foldMap and pass the interpreter
//    prog.foldMap(ConsoleInterpreter)

    
    //It is possible to create many other interpreters, depending on what you want to achieve
    //People say, that FreeMonad allows you write aspect oriented programs.
    //This is the aspect of testing, where you create interpeter which will just record the outputs so you can
    //compare it later in assertions with expected values
    
    //Tester is a helper alias representing Function from KevValue to Container of List, and returned value A
    //KV store represents the input which would be provided on given prompts (remember Ask(prompt: String)
    type Tester[A] = Map[String, String] => (List[String], A)

    //This is interpreter for testing
    object Test extends (Interaction ~> Tester) {
      override def apply[A](fa: Interaction[A]): Tester[A] = fa match {
        case Ask(prompt) => m => (List(), m(prompt)) // hmm  mmmm ???
        case Tell(message) => m => (List(message), ()) // hmm  mmmm ???
      }
    }
    
    //The tricky part is that if you want to run this interpreter in foldMap function, there is a constraint on it.
    //The constraint says that Tester must be a Monad. This is why this implicit implementation must live here:
    implicit val testerMonad = new Monad[Tester] {
      override def flatMap[A, B](fa: Tester[A])(f: (A) => Tester[B]): Tester[B] = m => { //WTF ???
        val (o1, a) = fa(m)
        val (o2, b) = f(a)(m)
        (o1 ++ o2, b)
      }
      override def pure[A](a: A): Tester[A] = _ => (List(), a)

      //hmm, WTF ???
      override def tailRecM[A, B](a: A)(f: (A) => Tester[Either[A, B]]): Tester[B] = defaultTailRecM(a)(f)
    }

    //WTF ???
    implicit val recursiveTailRecMTester: RecursiveTailRecM[Tester] = RecursiveTailRecM.create[Tester]

    //And this is you you can run the prog using created Test interpreter
    //As you can see this interpreter given the program will produce the Tester function.
    //This function can be used to test behaviour of prog
    val tester: Tester[Unit] = prog.foldMap(Test)
    
    //first - create input, output map
    //This will be used to feed the created tester function
    val `Q&A`: Map[String, String] = Map(
      "What is your first name?" -> "Harry",
      "What is your last name?" -> "Potter"
    )
    
    //and test it
    tester(`Q&A`) mustBe (List("Hello Harry Potter!"), ())
  }
}
