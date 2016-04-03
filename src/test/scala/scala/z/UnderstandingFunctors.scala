package scala.z

class UnderstandingFunctors extends ynfrastructure.Spec{

  "Simple fun with functors" in {

    import scalaz.Functor
//    Functor[Option].map(Some(1))(_+2) // WTF is 'F' ? : 'could not find implicit value for parameter F: scalaz.Functor[Option]'
    import scalaz.std.option._ //aha, we must import implicit instance of scalaz.Functor[Option]
    Functor[Option].map(Some(1))(_+2) mustBe  Some(3)

    import scalaz.std.list._ //aha, we must import implicit instance of scalaz.Functor[Option]
    Functor[List].map(List(1,2,3))(_+2) mustBe  List(3,4,5)
  }

  "custom funcor for set" in {
    import scalaz._

    implicit val setFunctor = new Functor[Set] {
      override def map[A, B](fa: Set[A])(f: (A) => B): Set[B] = fa.map(f)
    }
    Functor[Set].map(Set('siala, 'baba, 'mak))(_.name.toUpperCase) mustBe Set("SIALA", "BABA", "MAK")
  }

  "composing functors" in {
    import scalaz._
    import scalaz.std.list._
    import scalaz.std.option._

    implicit val looFunctor = Functor[List] compose Functor[Option] compose Functor[Option]

//    Functor[List[Option[Option]]] //booo!
    type LOO[X] = List[Option[Option[X]]]
    Functor[LOO].map(List(
      Some(Some(1)),
      Some(None),
      None
    )
    )(_ + 10) mustBe List(
      Some(Some(11)),
      Some(None),
      None
    )

    //or using kind projector

    Functor[λ[a => List[Option[Option[a]]]]].map(List(
      Some(Some(1)),
      Some(None),
      None
    )
    )(_ + 10) mustBe List(
      Some(Some(11)),
      Some(None),
      None
    )

  }

}
