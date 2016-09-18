package howitworks.scalacheck
import org.scalacheck
import org.scalacheck.Test.{Parameters, Result}
import org.scalacheck._

class ScalaCheckReadMeFirst extends ynfrastructure.Spec {

  "how does primitive blocks of scalacheck work" in {

    //What are Generators

    //Generator is simply trait with main functionality of generating some, random values of type T
    //The essence of Gen trait looks like
    new {
      //It does nothing than just generating values of type T based on some settings (genParameters)
      //If settings are to much restricted and there is no way to generate T instance, the None is returend
      //(Lists with size < 0 - no way to do this, this is why Option is used here)
      trait Gen[T] {
        def apply(genParameters: Gen.Parameters): Option[T]
      }
    }

    //In order to create generators, you can use convenient constructors
    val gen123: Gen[Int] = Gen.oneOf(1,2,3) //this will create generator which can generate only 3 numbers

    //let's test it
    List.fill(1000)(gen123.apply(Gen.Parameters.default)) must contain only (Some(1), Some(2), Some(3))

    //there are default generators defined by scalacheck

    val stringGen: Gen[String] = Gen.alphaStr
    val upperCharGen: Gen[Char] = Gen.alphaUpperChar

    //Generators can be composed and used to compose more complex instances
    //Thanks to that custom objects can be generated:

    case class Baby(name: String, age: Int)

    val genBaby: Gen[Baby] = for {
      nameFirstLetter <- upperCharGen
      restOfName <- stringGen
      name = nameFirstLetter + restOfName
      classRoomNo <- gen123
    } yield Baby(name, classRoomNo)

    //usage is as previously
    genBaby(Gen.Parameters.default)


    //Generating dynamic structures is possible

    //This will generate list of Baby instances
    val listGen: Gen[List[Baby]] = Gen.listOf(genBaby)

    //How long would be the list?
    //The max size of list comes from Gen.Parameters
    //it defaults to 100

    val someBabys: Option[List[Baby]] = listGen(Gen.Parameters.default)
    someBabys.value.length must be <= 100 //this assrtion is very waek, I know ...

    //It's possible to change default size param
    //... I don't know why it is not just a case class .....
    object CustomGenParams extends Gen.Parameters.Default {
      override val size: Int = 10
    }

    val someSmallListOfBabys: Option[List[Baby]] = listGen(CustomGenParams)
    someSmallListOfBabys.value.length must be <= 10 //this assrtion is very waek, I know ...

    //Another way to generate customary sized list is to generate size and use Gen.listOfN
    val genSmallListOfBabys: Gen[List[Baby]] = for {
      num <- Gen.posNum[Int]
      size = num%10
      list <- Gen.listOfN(size, genBaby)
    } yield list



    //What are Props

    //Prop can be thought as wrapped function f of shape
    //(Smth0, Smth1, ...) => Boolean
    //Such functions represent if for given instances Smth0...SmthN property defined in body of this function holds
    //Prop contains as well generators of Smth0, Smth2, etc.
    //Thanks to that it can generate inputs for function and test what it returns

    //Creation of Prop instances
    //Prop instances can be created using Prop.forAll constructors
    //Let's create few instances

    val propStartsWith: Prop = Prop.forAll { (a: String, b: String) =>
      (a + b).startsWith(a)
    }

    val propContains: Prop = Prop.forAll { (a: String, b: String) =>
      (a + b).contains(a) && (a + b).contains(b)
    }

    //Note that when defining Prop you must provide as well the way how to generate inputs
    //This is done using implicit parameter, so in previous examples we didn't provided such generators explicitly
    //Let's now create custom generator and create one Prop which will use this generator
    //Not sure why, but Prop.forAll requires not directly Gen instances but Gen wrapped into Arbitrary
    //Arbitrary is nothing more than Gen


    //Hmm, not possible to just new when constructing Arbitrary
    //implicit val stringArbitrary: Arbitrary[String] = new Arbitrary[String]{
    //  override val arbitrary: Gen[String] = ???
    //}
    //We must use constructor from companion object
    new { //local scope, otherwise below implicit will be piced up bu above Props
      implicit val customStringArbitrary: Arbitrary[String] = Arbitrary[String](Gen.alphaStr)

      //this prop will use customStringArbitrary
      val propContains: Prop = Prop.forAll { (a: String, b: String) =>
        (a + b).contains(a) && (a + b).contains(b)
      }
    }


    //Checking properties

    //Man can check if defined properties hold.
    //There is main method for it (primitive) which is used by all convenience methods for doing it.
    //It takes Prop instanca and some Parameters telling for example how many samples need to be
    //generated in order to conclude that such property holds.

    //Default parameters lives here
    val params = Test.Parameters.default

    //This is how man can run checking.
    //Check will generate samples using generators defined in Prop and  run Prop's testing routine, collect all results them
    val result: Result = Test.check(params, propStartsWith)

    result.passed mustBe true
    result.discarded mustBe 0
    result.status mustBe Test.Passed

    //End of intro
  }

}
