package cat.s

import cats.Id
import cats.data.{Kleisli, Reader}


class ReaderDemo extends ynfrastructure.Spec {

  "hello reader" in {

    //Let's say there are some functions from
    // f: String => Whatever

    val size: String => Int = _.size
    val ovalLettersCount: String => Int = _.count(List('q', 'Q', 'o', 'O', 'p', 'P', 'd', 'D', 'R', 'a','A','g','b').contains(_))
    val lettersDistribution: String => Map[Char, Int] = _.foldLeft(Map[Char, Int]())((acc, curr) => acc.updated(curr, 1 + acc.getOrElse(curr, 0)))

    //and you would like to create 3rd function based on 'returned values' of these above functions
    //standard approach is:
    val stringAnalysis: String => String = {s =>
      val sSize = size(s)
      val sOvalLettersSize = ovalLettersCount(s)
      val sLetterDistribution = lettersDistribution(s).toList.sortBy(x => (x._1.toUpper, x._1)).mkString(", ")
      s"The string '${s}' contains $sSize letters, $sOvalLettersSize oval letters and in general here this is distribution of letters: $sLetterDistribution"
    }

    //let's test it
    val exampleIn = "what are reader monads all about"
    val expectedOut = "The string 'what are reader monads all about' contains 32 letters, 11 oval letters and in general here this is distribution of letters: ( ,5), (a,6), (b,1), (d,2), (e,3), (h,1), (l,2), (m,1), (n,1), (o,2), (r,3), (s,1), (t,2), (u,1), (w,1)"
    //it works
    stringAnalysis(exampleIn) mustBe expectedOut

    //using writer monad you can create stringAnalysis it in different way:
    //Reader[A, B] represents function A => B
    //flatMap and map works on returned value B

    val stringAnalysis2: Reader[String, String] = for {
      input <- Reader[String, String](identity)
      size <- Reader(size)
      ovalCount <- Reader(ovalLettersCount)
      dist <- Reader(lettersDistribution)
      distFormatted = dist.toList.sortBy(x => (x._1.toUpper, x._1)).mkString(", ")
    } yield s"The string '$input' contains $size letters, $ovalCount oval letters and in general here this is distribution of letters: $distFormatted"

    stringAnalysis2.run(exampleIn) mustBe expectedOut

  }
}
