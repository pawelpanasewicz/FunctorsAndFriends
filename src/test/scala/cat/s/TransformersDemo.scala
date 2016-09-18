package cat.s

import java.util.concurrent.Executors

import cats.data.{OptionT, Writer, Xor}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.{-\/, WriterT, \/-}


class TransformersDemo extends ynfrastructure.Spec {

  "motivation" in {

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

    //it works, but it is hard write code such way
    Await.result(xy, 500 millis).value mustBe "Nested Monads"

    //Let's rewrite it using Monad Transformers.
    import cats.instances.future._ //cats.Functor[Future] is needed

    //less caracters, looks cleaner, but result type is different
    val xyT: OptionT[Future, String] = for {
      x <- OptionT(x)
      y <- OptionT(y)
    } yield s"$x $y"

    //this is how you can get wrapped monad out of this transofmer
    val xy2 = xyT.value

    //and works as expected
    Await.result(xy2, 500 millis).value mustBe "Nested Monads"


    //Now lets see some more complex example when dealing with Future[Option[A]]

    //given a functinon f: A => Future[Option[A]]
    //it's not important what it does, just focus on signature
    def distributionOfLetters(s: String): Future[Option[String]] = Future{
      if(s == "") None
      else Some (
        s.foldLeft(Map[Char, Int]()) { case (acc, c) => acc.updated(c, 1 + acc.getOrElse(c, 0)) }
          .toList
          .sortBy(_._1)
          .mkString(s"distribution of letters of '$s' is: ", ", ", "")
      )
    }

    //as well, given some for comprehension when second value depends on first one
    //note how ugly it is:
    import cats.syntax.traverse._
    import cats.instances.future._
    import cats.instances.option._

    val stringAndDescription: Future[Option[(String, String)]] = for {
      x: Option[String] <- x
      tmp1: Option[Future[Option[String]]] = x.map(x => distributionOfLetters(x))
      tmp2: Future[Option[Option[String]]] = tmp1.sequenceU
      tmp3: Future[Option[String]] = tmp2.map(_.flatten)
      desc: Option[String] <- tmp3
    } yield {
      for {
        x <- x
        desc <- desc
      } yield (x, desc)
    }

    //it works
    Await.result(stringAndDescription, 500 millis).value mustBe ("Nested","distribution of letters of 'Nested' is: (N,1), (d,1), (e,2), (s,1), (t,1)")

    //Ok, let's rewrite it using monad transformers:
    val stringAndDescription2: OptionT[Future, (String, String)] = for {
      x <- OptionT(x)
      desc <- OptionT(distributionOfLetters(x))
    } yield(x,desc)

    //and it works like previously
    Await.result(stringAndDescription2.value, 500 millis).value mustBe ("Nested","distribution of letters of 'Nested' is: (N,1), (d,1), (e,2), (s,1), (t,1)")

  }

}
