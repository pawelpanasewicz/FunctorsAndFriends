package howitworks.scalacheck

import org.scalacheck.{Arbitrary, Gen}
import ynfrastructure.Spec

class SomethingCheckingSpec extends Spec {

  /**
    * This is how you can customize Checks
    */
  implicit val config = PropertyCheckConfig(
    minSuccessful = 100,
    maxDiscarded = 5000,
    minSize = 0,
    maxSize = 100,
    workers = 1
  )

  "checkSomething" in forAll { (something: Something) =>
    test(something)
  }


  private def test(something: Something) = {
    whenever(something.value.length == 0) {
      something.value mustBe ""
    }

//    whenever(something.value.length > 0) {
//      Something((something.value + "aa").toUpperCase) mustBe Something(something.value.toUpperCase + "AA")
//    }
  }

  case class Something(value: String)

  implicit lazy val arbitrarySomething = Arbitrary[Something](somethingGen)

  lazy val somethingGen: Gen[Something] = for {
      string <- Gen.alphaStr
      num <- Gen.numStr
      value <- Gen.frequency[String]( (5, string), (1, num), (10, ""))
  } yield Something(value)
}
