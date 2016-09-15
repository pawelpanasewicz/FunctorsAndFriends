package cat.s

import cats.FlatMap
import cats.data.Kleisli


class HelloKleisli extends ynfrastructure.Spec {

  "hello kleisli" in {

    //given functions in type signare: A => F[B] and B => F[C]
    //you may want to create 3rd function A => F[C]

    val fa: String => Option[Int] = s => if(s.isEmpty) None else Some(s.size)
    val fb: Int => Option[Double] = i => if(i == 0) None else Some(102.0/(i.toDouble))
    val fc: String => Option[Double] = s => fa(s).flatMap(fb)

    fc("abc") mustBe Some(102.0/3.0)
    fc("") mustBe None

    //or you can wrap values inside kleisli:

    //needed by Kleisli
    implicit val faltMapOption: FlatMap[Option] = cats.instances.option.catsStdInstancesForOption

    val faK = Kleisli[Option, String, Int](fa)
    val fbK = Kleisli[Option, Int, Double](fb)
    val fcK = faK.andThen(fbK)
    val fcK2 = fbK.compose(faK)  //the same as fcK

    fcK.run("abc") mustBe Some(102.0/3.0)
    fcK.run("") mustBe None
    fcK2.run("abc") mustBe Some(102.0/3.0)
    fcK2.run("") mustBe None
  }

}
