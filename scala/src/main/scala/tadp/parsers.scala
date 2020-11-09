package tadp
import scala.util._

package object parsers {
  val anyChar: Parser[Char] = {
    case "" => Failure(new ParserError)
    case nonEmpty => Success(ParserResult(nonEmpty.head, nonEmpty.tail))
  }

  val char: Char => Parser[Char] = target => anyChar.satisfies(_ == target)

  val digit: Parser[Int] = anyChar.satisfies(_.isDigit).map(_ - 48)

  val string: String => Parser[String] = target => target.map{ char(_).map(_.toString)}.reduceLeft{(a, b) => (a <> b).map{ case (c1, c2) => c1 + c2 }}

  val sign: Parser[Int] = char('-').opt().map{opt => if (opt.isEmpty) 1 else -1}

  val unsigned: Parser[Int] = digit.+.map { case digits => digits.reduceLeft{ _ * 10 + _ } }

  val integer: Parser[Int] =  (sign <> unsigned).map { case (sign, unsigned) => sign * unsigned }

  val fractional: Parser[Double] = (char('.') ~> digit.+).map{_.map{_.toDouble}.foldRight(.0)((a, b) => (a + b) / 10 )}

  val double: Parser[Double] = (sign <> unsigned <> fractional.opt()).map { case ((sign, real), fractional) => sign * (real.toDouble + fractional.getOrElse(.0)) }
}
