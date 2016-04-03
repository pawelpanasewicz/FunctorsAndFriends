package cat.s

class UnderstandingFunctors extends ynfrastructure.Spec {

  "Functor" - {
    import cats.Functor
    "map" in {
      import cats.std.option._
      //import standard instances of typeclasses for option. This includes functor instance for option

      Functor[Option].map(Some("sialababamak"))(_.toUpperCase()) mustBe Some("SIALABABAMAK")

      import cats.std.list._
      //import standard instance for List

      Functor[List].map(List(1, 2, 3))(_ + 1) mustBe List(2, 3, 4)

      //there is no standard functor instance for Set - this i why below doesn't compile
      //      type S[X] = Set[List[X]]
      //      implicit val sFunctor = Functor[Set] compose Functor[List]
      //      Functor[S].map(Set(List(1,2,3), Nil, List(10,10, 10)))(_.map(_ + 1)) mustBe Set(List(1,2,3), Nil, List(10,10, 10))
    }

    "custom functor for Set" in {

      //Functor[Set].map(Set(1,2,3))(_*2)  // will not compile because there is no functor for Set yet in cats, we must define it by hand:

      implicit val setFunctor = new Functor[Set] {
        override def map[A, B](fa: Set[A])(f: (A) => B): Set[B] = fa.map(f)
      }

      Functor[Set].map(Set(1, 2, 3))(_ * 2) mustBe Set(2, 4, 6) //and now we can use it
    }

    "composing functors" in {
      import cats.std.list._
      import cats.std.option._

      //Functor[List[Option]] // won't compile with error 'List[Option] takes no type parameters, expected: one'. Let's fix it:

      type LO[X] = List[Option[X]] //booo! extra type needs to be provided here

      //Functor[LO] //booo! could not find implicit value for parameter instance: cats.Functor[LO]; let's fix it:
      implicit val loFunctor: Functor[LO] = Functor[List] compose Functor[Option]

      Functor[LO] //works! It sees implicit loFunctor and thus can use Functor.apply to get this functor

      Functor[LO].map(List(Some(1), Some(2), None, Some(4)))(_ + 1) mustBe List(Some(2), Some(3), None, Some(5))

      //nice, now we can map over values deep inside containers for which there exist specific functors:

      val complexContainer = List(
        List(
          Some(Map(1 -> false, 2 -> true)),
          None
        ),
        List(
          Some(Map(10 -> true, 11 -> true))
        )
      )

      type MValue[X] = Map[Int, X] //booo! extra type needs to be provided here

      //there is no functor for Map, let's create such. It's needed in order to create complexContainerFunctor
      implicit val mvalueFunctor = new Functor[MValue] {
        override def map[A, B](fa: MValue[A])(f: (A) => B): MValue[B] = fa.mapValues(f)
      }

      implicit val complexContainerFunctor = Functor[List] compose Functor[List] compose Functor[Option] compose Functor[MValue]

      //and the complexContainerFunctor in action:
      complexContainerFunctor.map(complexContainer)(_ || true) mustBe List(
        List(
          Some(Map(1 -> true, 2 -> true)),
          None
        ),
        List(
          Some(Map(10 -> true, 11 -> true))
        )
      )
    }

    "more fun with functors - let's change sub container" in {
      import cats.std.list._
      import cats.std.option._

      val complexContainer = List(
        List(
          Some(Map(1 -> false, 2 -> true)),
          None
        ),
        List(
          Some(Map(10 -> true, 11 -> true))
        )
      )

      //can we replace Map[Int->Boolean] with Set[(Int, String)] inside deep structure ?
      def transformMap(m: Map[Int, Boolean]): Set[(Int, String)] = m.mapValues(if (_) "Yes" else "No").toSeq.toSet

      //implicit val specialTaskFunctor = new Functor[List[List[Option]]] =   ... eee why not using Functor.comppose for it :)
      implicit val specialTaskFunctor = Functor[List] compose Functor[List] compose Functor[Option]

      type LLO[X] = List[List[Option[X]]] //booo!
      Functor[LLO].map(complexContainer)(transformMap) mustBe List(
        List(
          Some(Set(1 -> "No", 2 -> "Yes")), // Set
          None
        ),
        List(
          Some(Set(10 -> "Yes", 11 -> "Yes")) // Set
        )
      )
    }

    "kind projections" in {
      import cats.std.list._
      import cats.std.option._
      implicit val looFunctor = Functor[List] compose Functor[Option] compose Functor[Option]

      //      Functor[List[Option[Option]]]  //will not compile, because of 'List[Option[Option]] takes no type parameters, expected: one'

      //      booo! wee need extra type here
      type LOO[X] = List[Option[Option[X]]]
      Functor[LOO]

      //KindProjector is here to rescue us from 'boo!'
      //Functor[List[Option[Option[?]]]] // no no no, this is not going to help ... see next line
      Functor[Lambda[a => List[Option[Option[a]]]]]
    }

    "experiment - auto composing functors" in {
      import cats.std.list._
      import cats.std.option._
      import scala.language.higherKinds

      //      implicit def autoComposedFunctor[A[_], B[_]](implicit functorA: Functor[A], functorB: Functor[B]) = functorA compose functorB
      //or prettier:
      implicit def autoComposedFunctor[A[_] : Functor, B[_] : Functor]: Functor[Lambda[a => A[B[a]]]] = Functor[A] compose Functor[B]
      Functor[Lambda[a => List[Option[a]]]] //here works

//        implicit val x: Functor[({type Λ$[a] = scala.List[_root_.scala.Option[a]]})#Λ$] = Functor[Lambda[a => List[Option[a]]]]

      //      Functor[Lambda[a => List[List[Option[a]]]]]  //this doesn't want to cooperate :/

    }
  }
}
