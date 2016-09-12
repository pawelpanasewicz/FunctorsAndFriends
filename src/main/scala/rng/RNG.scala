package rng

trait RNG {
  def run: Long
  def next: RNG
}

//based on spire.random.rng.Lcg64
case class Lcg64(seed: Long) extends RNG {
  override lazy val run: Long = 6364136223846793005L * seed + 1442695040888963407L
  override lazy val next: RNG = Lcg64(run)
}

object Lcg64 {
  def fromSeed(seed: Long = System.currentTimeMillis()): Lcg64 = Lcg64(seed)
}

//based on spire.random.rng.Cmwc5
case class Cmwc5(x0: Long, x1: Long, x2: Long) extends RNG {
  override lazy val next: Cmwc5 = Cmwc5(x0 = x1, x1 = x2, x2 = v1)
  override lazy val run: Long = (x1 + x1 + 1) * v1

  private lazy val t: Long = x0 ^ (x0 >>> 7)
  private lazy val v1 = (x2 ^ (x2 << 6)) ^ (t ^ (t << 13))
}

object Cmwc5 {
  def fromSeed(seed: Long = System.currentTimeMillis()): Cmwc5 = {
    val rng0 = Lcg64.fromSeed(seed)
    val x0 = rng0.run
    val x1 = rng0.next.run
    val x2 = rng0.next.next.run
    Cmwc5(x0, x1, x2)
  }
}