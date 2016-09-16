package simulacru.m


import simulacrum._

//This is just a behaviour, some type will have it and other won't
@typeclass trait CanTruthy[A] {
  def truthy(a: A): Boolean
}

//this is just another behaviour, some type will have it, others won't
@typeclass trait MakeBigger[A] {
  /**
    * @return something bigger than 'a'
    */
  def makeBigger(a: A): A
}

class TypeclassDemo extends ynfrastructure.Spec {

  "stringTruthy" in {
    implicit val x = TypeClassInstances.string.stringTruthy
    CanTruthy[String] truthy "" mustBe true
    CanTruthy[String].truthy("asdf") mustBe false

    import CanTruthy.ops._
    "".truthy mustBe true
    "siala baba mak".truthy mustBe false
  }

  "stringTruthy2" in {
    implicit val x = TypeClassInstances.string.yesStringTruthy
    CanTruthy[String] truthy "" mustBe false
    CanTruthy[String].truthy("asdf") mustBe false
    CanTruthy[String].truthy("yes") mustBe true
    CanTruthy[String].truthy("YES") mustBe true

    import CanTruthy.ops._
    "YES".truthy mustBe true
    "siala baba mak".truthy mustBe false
  }

  "string can be made bigger" in {
    import TypeClassInstances.string.stringMakeBigger
    import MakeBigger.ops._

    "I am the small string".makeBigger mustBe "I AM THE BIGGER MUCH MUCH MUCH BIGGER STRING AAAAA"
  }


  object TypeClassInstances {

    object string {
      implicit val stringTruthy: CanTruthy[String] = ct(_.isEmpty)
      implicit val yesStringTruthy: CanTruthy[String] = ct(_.toLowerCase() == "yes")
      implicit val stringMakeBigger: MakeBigger[String] = bigger(_.toUpperCase.replaceAllLiterally("SMALL", "BIGGER MUCH MUCH MUCH BIGGER") + " AAAAA")
    }

    object int {
      implicit val intTruthy: CanTruthy[Int] = ct(_ == 0)
      implicit val intTruthy2: CanTruthy[Int] = ct(_ >= 0)
    }

    //convinience method for creating CanTruthy
    def ct[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
      override def truthy(a: A): Boolean = f(a)
    }
    //convinience method for creating MakeBigger
    def bigger[A](f: A => A): MakeBigger[A] = new MakeBigger[A] {
      override def makeBigger(a: A): A = f(a)
    }
  }
}

