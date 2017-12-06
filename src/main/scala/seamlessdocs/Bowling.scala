package seamlessdocs

object Bowling {
  def scoring(line: String): Int = {
    val frames = line.split(' ')

    def scoreSub(i: Int, // frame index
                 bonus1: Int, // how many times for next throw
                 bonus2: Int, // how many times for 2nd to next throw
                 ret: Int // accumulated score
                ): Int = {
      if (i == frames.length)
        ret
      else if (bonus1 == 0) {
        throw new IllegalArgumentException(s"Extra frame detected, frame number ${i + 1}")
      }
      else {
        val (nb1, nb2, s) =
         frames(i).toList match {
          case List('X') =>
            (bonus2 + 1, 2, 10 * bonus1)

          case List(a, '/') if toScore(a) >= 0 =>
            (2, 1, toScore(a) * bonus1 + (10 - toScore(a)) * bonus2)

          case List(a, b) if toScore(a) + toScore(b) <= 10 =>
            (1, 1, toScore(a) * bonus1 + toScore(b) * bonus2)

          case List(a) if toScore(a) >= 0 =>
            (1, 1, toScore(a) * bonus1)

          case x => throw new IllegalArgumentException(s"Ill-formatted frame $x")
        }

        if (i >= 9)
          scoreSub(i + 1, nb1 - 1, nb2 - 1, ret + s)
        else
          scoreSub(i + 1, nb1, nb2, ret + s)
      }
    }

    scoreSub(0, 1, 1, 0)
  }

  private def toScore(c: Char): Int = {
    c match {
      case a if a.isDigit => a - '0'
      case '-' => 0
      case _ => throw new IllegalArgumentException(s"Illegal symbol $c")
    }
  }
}
