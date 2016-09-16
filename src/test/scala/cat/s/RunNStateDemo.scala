package cat.s

import cats.data._
import rng.{Cmwc5, RNG}


class RunNStateDemo extends ynfrastructure.Spec {

  "run State[S,A] computation many times and get the last result" - {

    //some State returning random longs or another description of computation
    val nextLong: State[RNG, Long] = State[RNG, Long] { rng => (rng.next, rng.run) }
    val iterations = 1000000
    val expectedLong = -8889165323458747630L

    "flatMap many times" in {

      import cats.syntax.traverse._
      import cats.instances.list._
      //you want to run many times the same computation and get the last result along with last state
      //standard way of doing it:

      //this will create biiiig description,extra memory will be allocated
      def lastResult(nTimes: Int): State[RNG, Long] = List.fill(nTimes)(nextLong).sequenceU.map(xs => xs.last)
      val bigComputation = lastResult(iterations)
      val (bcState, bcResult) = bigComputation.run(Cmwc5.default).value
      bcResult mustBe expectedLong
      //~ 5 seconds on my lapop
    }

    "run in a loop" in {
      //below is faster solution, where you don't create such big computation
      //but you run it in a loop and feed back returned state
      //no extra memory is needed

      //runs n times State and returns the last (S,A)
      def runN[S, A](n: Int, state: State[S, A], s: S): (S, A) = {
        require (n > 0)
        (0 until n - 1).foldLeft(state.run(s).value)((b, _) => state.run(b._1).value)
      }

      //result is the same as previously
      runN(iterations, nextLong, Cmwc5.default)._2 mustBe expectedLong
      //~100ms on my laptop
    }

  }
}
