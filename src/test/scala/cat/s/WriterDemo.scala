package cat.s


import cats.Id
import cats.data.{Writer, _}

class WriterDemo extends ynfrastructure.Spec {

  implicit val stringSemigroup = cats.instances.string.catsKernelStdMonoidForString
  //import cats.instances.string._

  "hello writer" in {

    //create two instaces of writer
    val a: Writer[String, Int] = Writer("Got 1.", 1)
    val b: Writer[String, Int] = Writer("Got 2.", 2)

    //construct 3rd writer instance based two previous
    val c: Writer[String, Int] = for {
      _ <- Writer.tell("Adding two numbers.") //this creates additional Log message
      a <- a
      b <- b
      _ <- Writer.tell("Numbers added.") //this creates additional Log message
    } yield a + b

    //this is how you get the enclosed computation and logs
    val result = c.run
    result mustBe ("Adding two numbers.Got 1.Got 2.Numbers added.", 3)
  }
}
