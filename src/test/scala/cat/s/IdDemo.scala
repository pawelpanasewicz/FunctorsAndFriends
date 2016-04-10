package cat.s

class IdDemo extends ynfrastructure.Spec {

  "fun with id" in {

    import cats.Id

    val i1: Id[Int] = 10
    val i2: Int = 10
    i1 mustBe i2

    //Error type mismatch; found   : i1.type (with underlying type cats.Id[Int]) required: AnyRef
    //probably Int's and other not AnyRefs are special
//    theSameParentType[i1.type, i2.type, Int]
//    theSameParentType[i1.type, i2.type, Id[Int]]

    val s1: Id[String] = "10"
    val s2: String = "10"

    theSameParentType[s1.type, s2.type, String]
    theSameParentType[s1.type, s2.type, Id[String]]

  }
}
