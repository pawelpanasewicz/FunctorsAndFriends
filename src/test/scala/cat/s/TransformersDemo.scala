package cat.s

import java.util.concurrent.Executors

import cats.data.{OptionT, Xor}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.{-\/, \/-}


class TransformersDemo extends ynfrastructure.Spec {

  "Hello Monad Transformers" in {

    //given monad inside monad
    val x: Future[Option[String]] = Future{Some("Nested")}
    val y: Future[Option[String]] = Future{Some("Monads")}

    //and standard way of "composing" nested monads
    val xy = for {
      x: Option[String] <- x
      y: Option[String] <- y
    } yield {
      for {
        x: String <- x
        y: String <- y
      } yield s"$x $y"
    }

    Await.result(xy, 100 millis).value mustBe "Nested Monads"


    //Let's rewrite it using Monad Transformers.

    //these are the same but wrapped inside Monad Transformer
    val xT: OptionT[Future, String] = OptionT{x}
    val yT: OptionT[Future, String] = OptionT{y}

    //now you need Functor for future, which is required by transformer
    //this extra import solves it
    import cats.instances.future._

    //compare uppor 'for' with this one
    val xyT = for {
      x <- xT
      y <- yT
    } yield s"$x $y"

    //works as expected
    Await.result(xyT.value, 100 millis).value mustBe "Nested Monads"

    //Let's assume there is function from A => F[G[A]]
    //How to hadnle it in here ?

    def pretiffy(x: String): Future[Option[String]] = Future{if(x == "") None else Some(s">>>$x<<<")}

    val xyT2 = for {
      x <- xT
      y <- OptionT(pretiffy(x)) //or change put OptionT{...} to pretiffy
    } yield s"$x $y"

    //works like a charm
    Await.result(xyT2.value, 100 millis).value mustBe "Nested >>>Nested<<<"
  }

}
