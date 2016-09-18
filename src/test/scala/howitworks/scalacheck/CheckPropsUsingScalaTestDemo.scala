package howitworks.scalacheck

import org.scalacheck
import org.scalacheck.{Arbitrary, Gen, Prop, Shrink}
import org.scalactic.Prettifier
import org.scalactic.source.Position
import org.scalatest.enablers.CheckerAsserting
import org.scalatest.prop.{Configuration, GeneratorDrivenPropertyChecks}
import ynfrastructure.Spec

class CheckPropsUsingScalaTestDemo extends Spec {

  //This describes Property Driven Testing in scalatest using ScalaTest style
  //This approeach is known as Generator-driven property checks
  //There exist as well ScalaCheck-style - if interested read http://www.scalatest.org/user_guide/writing_scalacheck_style_properties


  //You can marriage scalatest and scalacheck extending PropertyChecks trait
  //Thanks to that 'forAll' functions are here and they allow you to run Prop objects from scalacheck library
  //Advantage is that they are nicely reported and automatically picked up when tests run (is sbt for example)

  //Disadvantage is that part of PropertyChecks is generated though it's hard to read its methods signatures.
  //Use intellij ctrl+f12 or ctrl+q and look for methods 'forAll[A](...) to see what they are
  //Or read below extracted signatures:


  new {

    //Below methods will create and run scalacheck.Props in one step

    //in body of these methods use normal scalatest matchers
    //they will throw exceptions as usual and scalatest will handle all of them for you
    //read http://doc.scalatest.org/3.0.0/#org.scalatest.prop.GeneratorDrivenPropertyChecks

    def forAll[A, ASSERTION](fun: A => ASSERTION)
                            (implicit config: PropertyCheckConfigurable,
                             arbA: Arbitrary[A],
                             shrA: Shrink[A],
                             asserting: CheckerAsserting[ASSERTION],
                             prettifier: Prettifier,
                             pos: Position
                            ): asserting.Result = ???

    def forAll[A, ASSERTION](nameA: String, configParams: PropertyCheckConfigParam*)
                            (fun: A => ASSERTION)
                            (implicit config: PropertyCheckConfigurable,
                             arbA: Arbitrary[A],
                             shrA: Shrink[A],
                             asserting: CheckerAsserting[ASSERTION],
                             prettifier: Prettifier,
                             pos: Position): asserting.Result = ???

    def forAll[A, ASSERTION](genA: Gen[A], configParams: PropertyCheckConfigParam*)
                            (fun: A => ASSERTION)
                            (implicit config: PropertyCheckConfigurable,
                             shrA: Shrink[A],
                             asserting: CheckerAsserting[ASSERTION],
                             prettifier: Prettifier,
                             pos: Position): asserting.Result = ???

    def forAll[A, ASSERTION](genAndNameA: (Gen[A], String),
                             configParams: PropertyCheckConfigParam*)
                            (fun: A => ASSERTION)
                            (implicit config: PropertyCheckConfigurable,
                             shrA: Shrink[A],
                             asserting: CheckerAsserting[ASSERTION],
                             prettifier: Prettifier,
                             pos: Position): asserting.Result = ???

    //and so on for Function2, Function3 etc
  }

  //This is the config how you can customize all Checks
  //For more info what they do read scala doc in instances of
  //PropertyCheckConfigParam
  implicit val config = PropertyCheckConfiguration(
    minSuccessful = 100,
    maxDiscardedFactor = 0.5,
    minSize = 0,
    sizeRange = 100,
    workers = 1
  )

  "string concatenation is associative" in forAll{(a: String, b: String, c: String) =>
    (a+b)+c mustBe a+(b+c)
  }

  //the same but with tagged parameters, better reporting in case of failure
  "string concatenation is associative for small string" in forAll("a", "b", "c"){(a: String, b: String, c: String) =>
    (a+b)+c mustBe a+(b+c)
  }

  //I am not big fun of 'whenever' and similar constructs, because it is easy to exhaust generators
  //In such situations lazy developers will tune generators to work harder ...
  //just to show how to pass config for one 'forAll' methods, here goes one example:
  //
  val bigMaxDiscardedFactor = maxDiscardedFactor(0.9999)
  "string concatenation is associative for  mid strings" in forAll("a", "b", "c", bigMaxDiscardedFactor){(a: String, b: String, c: String) =>
    whenever(a.size > 30){ //Booooo!
      (a+b)+c mustBe a+(b+c)
    }
  }

  //better way is to use size parameter, which will generate bigger items:
  val minSize100 = minSize(100)
  val sizeRange1000 = sizeRange(1000)
  "string concatenation is associative for big strings" in forAll("a", "b", "c", minSize100, sizeRange1000){(a: String, b: String, c: String) =>
    (a+b)+c mustBe a+(b+c)
  }


  //Let's test some custom objects

  case class Baby(name: String, age: Int) {
    def canEatBread: Boolean = age > 1
  }

  val genBaby1YearOld: Gen[Baby] = for {
    nameFirstLetter <- Gen.alphaUpperChar
    restOfName <- Gen.alphaStr
    name = nameFirstLetter + restOfName
    age <- Gen.const(1)
  } yield Baby(name, age)

  "check young baby can eat bread" in forAll(genBaby1YearOld) { baby =>
    baby.canEatBread mustBe false
  }

  val genBabyOlderThan1Year: Gen[Baby] = for {
    nameFirstLetter <- Gen.alphaUpperChar
    restOfName <- Gen.alphaStr
    name = nameFirstLetter + restOfName
    age <- Gen.oneOf(2,3)
  } yield Baby(name, age)

  //the same as above, but different Gen and it's tagged
  "check older baby can eat bread" in forAll((genBabyOlderThan1Year, "baby")) { baby =>
    baby.canEatBread mustBe true
  }

  //change to some assertions to see how errors are reported
}
