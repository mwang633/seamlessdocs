package seamlessdocs

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by mymil on 12/6/2017.
  */
class BowlingSpec extends FlatSpec with Matchers {
  "Bowling Score" should "score complete games" in {
    Bowling.scoring("X X X X X X X X X X X X") should be (300)

    Bowling.scoring("9- 9- 9- 9- 9- 9- 9- 9- 9- 9-") should be (90)

    Bowling.scoring("5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5") should be (150)
  }

  "Bowling Score partial" should "score partial games" in {
    Bowling.scoring("X X X X X X X X X X X") should be (290)

    Bowling.scoring("9- 9- 9- 9- 9- 9- 9- 9- 9-") should be (81)

    Bowling.scoring("5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/") should be (130)
  }

  "Bowling Score exceptions" should "capture invalid input" in {
    assert(
    intercept[IllegalArgumentException] {
      Bowling.scoring("A B C")
    }.getMessage.startsWith("Illegal symbol"))

    assert(
      intercept[IllegalArgumentException] {
        Bowling.scoring("93 51 99 88 77 66 56 75 11 11")
      }.getMessage.startsWith("Ill-formatted frame"))

    assert(
      intercept[IllegalArgumentException] {
        Bowling.scoring("93 51 99 88 77 66 56 75 11 11")
      }.getMessage.startsWith("Ill-formatted frame"))

    assert(
      intercept[IllegalArgumentException] {
        Bowling.scoring("9- 9- 9- 9- 9- 9- 9- 9- 9- 9- X")
      }.getMessage.startsWith("Extra frame detected"))

    assert(
      intercept[IllegalArgumentException] {
        Bowling.scoring("9- 9- 9- 9- 9- 9- 9- 9- 9- X 33 3")
      }.getMessage.startsWith("Extra frame detected"))
  }
}
