package howitworks.scalacheck

import org.scalacheck
import org.scalacheck.{Prop, Properties}

class CheckPropsUsingCheckers extends ynfrastructure.Spec {


  //given Props

  object CheckString extends Properties("String") {
    property("startsWith") = Prop.forAll { (a: String, b: String) =>
      (a + b).startsWith(a)
    }

    property("contains") = Prop.forAll {
      (a: String, b: String) => (a + b).contains(a) && (a + b).contains(b)
    }

    property("this should not hold") = Prop.forAll {
      (a: String, b: String) => false
    }
  }

  val propContains: Prop = Prop.forAll { (a: String, b: String) =>
    (a + b).contains(a) && (a + b).contains(b)
  }

  val stringAssociativityProp: Prop = Prop.forAll{ (a: String, b: String, c: String) =>
    (a+b)+c == a+(b+c)
  }

  import org.scalatest.prop.Checkers._ //or extend it

  "check props" in {
    // ! This will not fail tests in scala test
    // ! It only prints result in console
    CheckString.check()

    //TODO check props defined in CheckString

    check(propContains)
    check(stringAssociativityProp)
  }
}
