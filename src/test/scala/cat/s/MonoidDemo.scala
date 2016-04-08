package cat.s

import cats._
import org.scalactic.Equality

class MonoidDemo extends ynfrastructure.Spec {

  "clock monoid" in {
    implicit val __ = clockSemigrop

    Monoid[Clock].empty mustBe Clock.c12
    Monoid[Clock].combine(Clock.c1, Clock.c2) mustEqual Clock.c3
    Monoid[Clock].combine(Clock.c12, Clock.c2) mustEqual Clock.c2

    import cats.syntax.semigroup._
    Clock.c1 |+| Clock.c1 mustEqual Clock.c2  //at now (08.04.2016) it looks red in intellij, probably because it uses custom macros under to hood
  }

  trait Clock {
    def hour: Int
    override def toString = s"Clock($hour)"
  }

  object Clock {
    val c1 = new Clock {override def hour: Int = 1}
    val c2 = new Clock {override def hour: Int = 2}
    val c3 = new Clock {override def hour: Int = 3}
    //...
    val c11 = new Clock {override def hour: Int = 11}
    val c12 = new Clock {override def hour: Int = 12}

    def apply(i: Int): Clock = new Clock{
      override def hour: Int = i.abs%12
    }
  }

  val clockSemigrop: Monoid[Clock] = new Monoid[Clock] {
    override def empty: Clock = Clock.c12
    override def combine(x: Clock, y: Clock): Clock = Clock(x.hour + y.hour)
  }

  implicit lazy val clockEquality: Equality[Clock] = new Equality[Clock] {
    override def areEqual(a: Clock, b: Any): Boolean = b match {
      case c:Clock => a.hour==c.hour
      case _ => false
    }
  }

}
