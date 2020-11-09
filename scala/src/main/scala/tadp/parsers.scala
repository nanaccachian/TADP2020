package tadp
import scala.util._

case object anyChar extends Parser[Char] {
  override def apply(input: String): Try[ParserResult[Char]] =
    input match {
      case "" => Failure(new ParserError)
      case _ => Success(ParserResult(input.head, input.tail))
    }
}

package object parsers {
  // Detalle minimo de estilo: esto es lo mismo pero más compacto (no es necesario usar case object ni el match)
  val anyChar: Parser[Char] = {
    case "" => Failure(new ParserError)
    case nonEmpty => Success(ParserResult(nonEmpty.head, nonEmpty.tail))
  }
}

case class char(target: Char) extends Parser[Char] {
  override def apply(input: String): Try[ParserResult[Char]] = anyChar.satisfies(_ == target) (input)
}

case object digit extends Parser[Int] {
  override def apply(input: String): Try[ParserResult[Int]] = anyChar.satisfies(_.isDigit).map(_ - 48) (input)
}

case class string(target: String) extends Parser[String] {
  override def apply(input: String): Try[ParserResult[String]] = target.map{ char(_).map(_.toString)}.reduceLeft{(a, b) => (a <> b).map{ case (c1, c2) => c1 + c2 }} (input)
}

case object sign extends Parser[Int] {
  override def apply(input: String): Try[ParserResult[Int]] = char('-').opt().map{opt => if (opt.isEmpty) 1 else -1} (input)
}

case object unsigned extends Parser[Int] {
  override def apply(input: String): Try[ParserResult[Int]] =
    digit.+.map {
      case digits => digits.reduceLeft{ _ * 10 + _ }
    } (input)
}

case object integer extends Parser[Int] {
  override def apply(input: String): Try[ParserResult[Int]] =
    (sign <> unsigned).map {
      case (sign, unsigned) => sign * unsigned
    } (input)
}

case object double extends Parser[Double] {
  override def apply(input: String): Try[ParserResult[Double]] = {
    val fractionalParser: Parser[Double] = (char('.') ~> digit.+).map{_.map{_.toDouble}.foldRight(.0)((a, b) => (a + b) / 10 )}
    (sign <> unsigned <> fractionalParser.opt()).map { case ((sign, real), fractional) => sign * (real.toDouble + fractional.getOrElse(.0)) } (input)
  }
}
