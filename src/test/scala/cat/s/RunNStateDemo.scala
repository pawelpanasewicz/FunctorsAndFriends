package cat.s

import cats.data._
import rng.{Cmwc5, RNG}


class RunNStateDemo extends ynfrastructure.Spec {

  //Conclusion: don't use 'sequence' if you don't care about intermediate results

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
      //~80ms on my laptop
    }

    "run using stream and remember all results" in {

      val s = {
        var state = Cmwc5.default
        Stream.continually {
          val ret = state.run
          state = state.next
          ret
        }
      }


      s.apply(iterations-1) mustBe expectedLong
      //~77ms on my laptop

      //another approaches
      val s2 =  {
        var state = Cmwc5.default
        state #:: Stream.continually{
          state = state.next
          state
        }
      }

      s2.apply(iterations-1).run mustBe expectedLong

      val s3 =  {
        var state = nextLong.run(Cmwc5.default).value
        state #:: Stream.continually{
          state = nextLong.run(state._1).value
          state
        }
      }
      s3.apply(iterations-1)._2 mustBe expectedLong
    }

    "run using stream but drop all results" in {
      val s = {
        var state = Cmwc5.default
        Stream.continually {
          val ret = state.run
          state = state.next
          ret
        }
      }

      s.drop(iterations-1).head mustBe expectedLong
      //~71ms on my laptop
    }

  }
}
