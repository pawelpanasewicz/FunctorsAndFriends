package scala.z


class NaturalTransformationDemo extends ynfrastructure.Spec {

  "Natural Transformation explained" in {

    import scalaz._
    import Scalaz._


    //SpermCellOperator '~>'is just an alias to type scalaz.NaturalTransformation[F[_], G[_]]
    //It is just a fancy way of creating function accepting 'F[_]' and returning 'G[_]'
    //In other words NaturalTransformation is wrapped function f: F[_] => G[_]
    //F and G can be for example Option, List, Id, Either[String, ?], and so on
    //NaturalTransformation sometimes is called FunctorTransformer
    //...probably because F and G have the same type shape as F in functor (remember trait Functor[F[_]] {...})
    //There is big math concept behing NaturalTransformation https://en.wikipedia.org/wiki/Natural_transformation
    //...but it is not essential to use it in scala
    //In most cases there is developer's responsibility to write instances of NaturalTransformation
    //NaturalTransformation are required when implementing the run logic of Free Monads

    //let's create one NaturalTransformation
    val optionToList: Option ~> List = new NaturalTransformation[Option, List] {
      def apply[A](fa: Option[A]): List[A] = fa.map(x => List(x)).getOrElse(Nil)
    }

    //let's run it
    optionToList(Some(10)) mustBe List(10)
    optionToList(None) mustBe List()

    //more examples of NaturalTransformation are shown in tutorials for FreeMonads
  }
}
