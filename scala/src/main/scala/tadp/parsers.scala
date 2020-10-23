import parser._

package object parsers {
  def anyChar: Parser[Char] = Parser { input => ParserResult(input.head, input.tail) }

  def char(target: Char): Parser[Char] = Parser { input =>
    if (!input.head.equals(target))
      throw ParserError()
    ParserResult(input.head, input.tail)
  }

  def digit: Parser[Int] = Parser { input =>
    if (!input.head.isDigit)
      throw ParserError()
    ParserResult(input.head.toInt - 48, input.tail)
  }

  def string(target: String): Parser[String] = Parser { input =>
    val targetLength = target.length
    if (!input.substring(0, targetLength).equals(target))
      throw ParserError()
    ParserResult(target, input.substring(targetLength))
  }

  /*
    def integer(target: Int): Parser[Int] = Parser { input =>
      val targetLength = target.toString.length
      if (!input.substring(0, targetLength).equals(target.toString))
        throw ParserError()
      ParserResult(target, input.substring(targetLength))
    }
   */

  def integer: Parser[Int] = Parser { input =>
    var pivot = digit(input)
    var result = pivot
    var consumed = 0
    while (pivot.isSuccess) {
      result = pivot
      consumed = consumed * 10 + result.get.consumed
      pivot = digit(result.get.input)
    }
    ParserResult(consumed, result.get.input)
  }
}
