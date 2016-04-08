package simulacru.m


import simulacrum._

@typeclass trait CanTruthy[A] {
  def truthy(a: A): Boolean
}

class TypeclassDemo extends ynfrastructure.Spec {

  "stringTruthy" in {
    implicit val x = monoidInstances.stringTruthy
    CanTruthy[String] truthy "" mustBe true
    CanTruthy[String].truthy("asdf") mustBe false

    import CanTruthy.ops._
    "".truthy mustBe true
    "siala baba mak".truthy mustBe false
  }

  "stringTruthy2" in {
    implicit val x = monoidInstances.yesStringTruthy
    CanTruthy[String] truthy "" mustBe false
    CanTruthy[String].truthy("asdf") mustBe false
    CanTruthy[String].truthy("yes") mustBe true
    CanTruthy[String].truthy("YES") mustBe true

    import CanTruthy.ops._
    "YES".truthy mustBe true
    "siala baba mak".truthy mustBe false
  }

  object monoidInstances {

    def ct[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
      override def truthy(a: A): Boolean = f(a)
    }

    implicit val stringTruthy: CanTruthy[String] = ct(_.isEmpty)
    implicit val yesStringTruthy: CanTruthy[String] = ct(_.toLowerCase() == "yes")
    implicit val intTruthy: CanTruthy[Int] = ct(_ == 0)
    implicit val intTruthy2: CanTruthy[Int] = ct(_ >= 0)
  }

}

