package howitworks.scalacheck

import org.scalacheck
import org.scalacheck.{Arbitrary, Gen}

object CheckString extends scalacheck.Properties("String") {

  property("startsWith") = scalacheck.Prop.forAll { (a: String, b: String) =>
    (a + b).startsWith(a)
  }

  property("contains") = scalacheck.Prop.forAll {
    (a: String, b: String) => (a + b).contains(a) && (a + b).contains(b)
  }

}

object CheckSomething extends scalacheck.Properties("Something") {

  property("I am contains 'am'") = scalacheck.Prop.forAll { (value: String) =>
    Something(value).IAm.contains("am")
  }

  property("I am contains 'I'") = scalacheck.Prop.forAll { (something: Something) =>
    something.IAm.contains("I")
  }

  case class Something(value: String) {
    def IAm = "I am " + value
  }


  implicit lazy val arbitrarySomething = Arbitrary[Something](somethingGen)
  lazy val somethingGen: Gen[Something] = for {
    value <- Gen.alphaStr // suchThat ( x => x.size == 0)
  } yield Something(value)

}

