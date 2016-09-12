
import cats.data.StateT
import cats.{Applicative, Eval, Monad}
import rng.Cmwc5

class StateDemo extends ynfrastructure.Spec {

  "Hello World State[S,A]" in {

    //`State[S, A]` (later State) represents computation in form of function wih `S => (S, A)` signature
    // type S represents the state, and type A represents the computed value
    //`S   => (..., ...)`  - here 'S' is an argument of that function, it is called initial state, later refered as 's'
    //`... => (..., A)`    - here 'A' represents a computed value, which can be derived from S, and is part of returned tuple, later refered as 'a'
    //`... => (S , ...)`   - here 'S' in tuple represents new state derived from initial state. Later rerefed as 'newS'


    //Mandatory import
    import cats.data.State


    //Let's create useful State instance representing Random Number Generator
    val nextLong: State[Cmwc5, Long] = State[Cmwc5, Long]{rng => (rng.next, rng.run)}

    //This will be initial 's' - state which will be passed as argument to
    //previously created 'nextLong' State.
    val initialRNG = Cmwc5.fromSeed(123L)

    val expectedRandomLong = -9048883945571160925L
    val expectedRNG = Cmwc5(4609227408502094885L, -1867546576565752112L, -9087076133393902071L)

    //We run computation defined by State by calling functnion. 'run'.
    //run returns a tuple of 'newS' and value 'a'
    nextLong.run(initialRNG).value mustBe (expectedRNG, expectedRandomLong)

    //It's possible to get only 'newS' or 'a'
    nextLong.runA(initialRNG).value mustBe expectedRandomLong
    nextLong.runS(initialRNG).value mustBe expectedRNG


    //There are some helpers in companion object for creating State instances.

    //State.apply
    // State can be created using apply function
    val s0 = State[Int, String](i => {
      val a = (i * 113).toString
      val nextState = a.length
      (nextState, a)
    } )

    //State.get
    //It creates State representing computation in form of `s => (s, s)`
    //Computed value 'a' will be the same as provided initial state 's'
    //In other words - it copies state 's' into value 'a'
    //It's useful in for comprehensions (see later), to get value of state
    val s1: State[Long, Long] = State.get[Long]
    //Let's run it. Computed value and state are the same as initial state 10L
    s1.run(10L).value mustBe (10L, 10L)

    //State.set
    //It creates State representing computation in form of `_ => (s, ())`
    val s2: State[Long, Unit] = State.set(115L)
    //Let's run it. Provided 's' `10L` is ignored as expected!
    s2.run(10L).value mustBe (115L, ())

    //State.modify: s => (f(s), ())
    val s3: State[Int, Unit] = State.modify[Int](f = _ + 10)
    //Let's run it. As expected when given 3 as input state, s3 increased it to 13
    s3.run(3).value mustBe (13, ())

    //State.pure: s => (s, a)
    //Whatever the initial state will be, it will return this state along with provided 'a'
    //It's useful in computations where 'a' doesn't depend on 's'
    //It's helpfull if you want to lift ordinary value into State in for comprehension
    val s4: State[Long, String] = State.pure[Long, String]("a value")

    //Whatever the internal state would be, s4 will return value "state madness!"
    s4.run(1000L).value mustBe (1000L, "a value")

    //State.inspect: `s => (s, f(s))`, f is type of `S => A`
    //It's similar to State.get. It additionally applies f to s
    val s5 = State.inspect[Int, String](i => (i+1).toString)
    //Let's run it
    s5.run(113).value mustBe (113, "114")
  }

  "play with random number generator using State" in {
    import cats.data.State
    import rng.Cmwc5

    //State in cats is invariant so below won't compile
    //val randomLong: State[RNG, Long] = State[Cmwc5, Long]{ rng => (rng.next, rng.run)}

    //Create State instance
    val nextLong: State[Cmwc5, Long] = State[Cmwc5, Long]{rng => (rng.next, rng.run)}

    val initialRNG = Cmwc5.fromSeed(123L)
    val expectedRandomLong = -9048883945571160925L
    val expectedRNG = Cmwc5(4609227408502094885L,-1867546576565752112L, -9087076133393902071L)

    //This is how we run computation defined by State
    //Such computation returns a tuple of `last state` and returned `value`
    nextLong.run(initialRNG).value mustBe (expectedRNG, expectedRandomLong)
    //Here are faster ways of getting either `result of computation` or or `last state`
    nextLong.runA(initialRNG).value mustBe expectedRandomLong
    nextLong.runS(initialRNG).value mustBe expectedRNG


    //How monadically few state's can be composed:
    val nextTuple: State[Cmwc5, (Long, Long)] = for {
      r1 <- nextLong
      r2 <- nextLong
    } yield (r1, r2)

    val expectedTuple = (expectedRandomLong, 6542662225182871060L)
    val expectedRNG2 = Cmwc5(-1867546576565752112L,-9087076133393902071L,-3256245211058860652L)
    //Let's run it
    nextTuple.run(initialRNG).value mustBe (expectedRNG2, expectedTuple)

    lazy val roll1Dice: State[Cmwc5, Long] = for {
      r1 <- nextLong
      r1NoNegative = Math.abs(r1)
      // let's fair play - if 'r1NoNegative' is greater then max long dividable by 6 - try once again
      r2 <- if(r1NoNegative > Long.MaxValue - (Long.MaxValue % 6)) roll1Dice else State.pure[Cmwc5, Long](r1NoNegative)
    } yield (r2 % 6) + 1

    roll1Dice.runA(initialRNG).value mustBe 2

    import cats.instances.list._ //sequenceU needst List traverse instance
    import cats.syntax.traverse._ //this is needed by syntax for traverse

    val rollManyDices: State[Cmwc5, List[Long]] = List.fill(20)(roll1Dice).sequenceU

    rollManyDices.runA(initialRNG).value mustBe List(2, 5, 6, 1, 6, 5, 6, 5, 5, 3, 1, 4, 2, 4, 1, 1, 2, 6, 6, 4)
  }


  "Manipulate stack with State[Stack,A]" in {
    //based on this story: http://eed3si9n.com/herding-cats/State.html

    //given Stack
    trait Stack[A] {
      def pop: Stack[A]
      def push(a: A): Stack[A]
      def topItem: A
    }

    //...and the Stack's simple implementation
    case class SimpleStack[A](store: List[A]) extends Stack[A] {
      override def pop = if(store.isEmpty) sys.error("Empty stack") else SimpleStack(store.tail)
      override def push(a: A) = SimpleStack(a :: store)
      override lazy val topItem = if(store.isEmpty) sys.error("Empty stack") else store.head
    }

    import cats.data.State

    def pop[A]: State[Stack[A], A] = State(stack => (stack.pop, stack.topItem))
    def push[A](a: A): State[Stack[A], Unit] = State.modify(_.push(a))

    def stackManip: State[Stack[String], String] = for {
      _ <- push("Stack")
      _ <- push("This")
      _ <- push("dropped")
      _ <- push("Jaw")
      a1 <- pop
      a2 <- pop
    } yield s"$a1 $a2?"

    val initialStack = SimpleStack(List("is", "crazy!"))

    val stackAfterManipulation = SimpleStack(List("This", "Stack", "is", "crazy!"))

    stackManip.run(initialStack).value mustBe (stackAfterManipulation, "Jaw dropped?")
  }
}



