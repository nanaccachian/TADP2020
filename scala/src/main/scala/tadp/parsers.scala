import scala.util.Try

package object parsers {

  type Parser[A] = String => Try[ParserResult[A]]

  case class ParserResult[T](consumed: T, input: String)
  case class ParserError() extends RuntimeException

  def tryParse[T](method: => ParserResult[T]): Try[ParserResult[T]] = Try {
    method
  } recover {
    case _ => throw ParserError()
  }

  anyChar: Parser[Char]
  def anyChar(input: String) = tryParse { ParserResult(input.head, input.tail) }

  char: (Char => Parser[Char])
  def char(target: Char)(input: String) = tryParse {
    if (!input.head.equals(target))
      throw ParserError()
    ParserResult(input.head, input.tail)
  }

  digit: Parser[Int]
  def digit(input: String) = tryParse {
    if (!input.head.isDigit)
      throw ParserError()
    ParserResult(input.head.toInt - 48, input.tail)
  }

  string: (String => Parser[String])
  def string(target: String)(input: String) = tryParse {
    val targetLength = target.length
    if (!input.substring(0, targetLength).equals(target))
      throw ParserError()
    ParserResult(target, input.substring(targetLength))
  }

  integer: (Int => Parser[Int])
  def integer(target: Int)(input: String) = tryParse {
    val targetLength = target.toString.length
    if (!input.substring(0, targetLength).equals(target.toString))
      throw ParserError()
    ParserResult(target, input.substring(targetLength))
  }
}
