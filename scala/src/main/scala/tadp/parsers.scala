package tadp
import scala.math._
import scala.util._

case object anyChar extends Parser[Char] {
  override def apply(input: String): Try[ParserResult[Char]] =
    input match {
      case "" => Failure(new ParserError)
      case _ => Success(ParserResult(input.head, input.tail))
    }
}

case class char(target: Char) extends Parser[Char] {
  override def apply(input: String): Try[ParserResult[Char]] = anyChar.satisfies(_ == target)(input)
}

case object digit extends Parser[Int] {
  override def apply(input: String): Try[ParserResult[Int]] = anyChar.satisfies(_.isDigit).map(_ - 48)(input)
}

case class string(target: String) extends Parser[String] {
  override def apply(input: String): Try[ParserResult[String]] = Try {
    val out = target.foldLeft(input)((output: String, ch: Char) => {
      char(ch)(output).get.output
    })
    ParserResult(target, out)
  }
}

case object integer extends Parser[Int] {
  override def apply(input: String): Try[ParserResult[Int]] = {
    val firstParse = char('-').opt()(input)
    val sign = if (firstParse.get.consumed.isEmpty) 1 else -1
    digit.+(firstParse.get.output).map( result => result.copy(consumed = result.consumed.reduceLeft((a, b) => a * 10 + b) * sign))
  }
}

case object double extends Parser[Double] {
  override def apply(input: String): Try[ParserResult[Double]] =
    (integer <> (char('.') <> integer).opt()) (input) match {
      case Success(ParserResult((real, tFractional), output)) if tFractional.nonEmpty =>
        tFractional.get match {
          case (_, fractional) if fractional >= 0 => Success(ParserResult(real + fractional / pow(10, fractional.toString.length) * real.sign, output))
          case _ => Failure(new ParserError)
        }
      case Success(ParserResult((real, _), output)) => Success(ParserResult(real, output))
      case _ => Failure(new ParserError)
    }
}
