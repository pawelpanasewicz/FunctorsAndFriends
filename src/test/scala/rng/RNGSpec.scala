package rng

class RNGSpec extends ynfrastructure.Spec {

  "Lcg64 is immutable Random Number Generator" in {
    val rng = Lcg64.fromSeed(10L)
    rng.run mustBe -8702919015481313007L
    rng.run mustBe rng.run
    rng.run mustBe -8702919015481313007L
    val rng2 = rng.next
    rng2 mustBe Lcg64(-8702919015481313007L)
    rng2.run mustBe 3298011952073619532L
  }

  "Cmwc5 is immutable Random Number Generator" in {
    val rng = Cmwc5.fromSeed(10L)
    rng.run mustBe -1671356653131063736L
    rng.run mustBe rng.run
    rng.run mustBe -1671356653131063736L
    val rng2 = rng.next
    rng2 mustBe Cmwc5(3298011952073619532L,658254279741938347L,2979551427464582536L)
    rng2.run mustBe 8294848756559657004L
  }

}
